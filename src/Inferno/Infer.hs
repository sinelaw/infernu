{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Inferno.Infer
    ( runTypeInference
    , test
    , Pretty(..)
    , pretty
    , getAnnotations
    , minifyVars
    , TypeError
    )
    where


import           Control.Monad             (foldM, forM)
import           Data.Foldable             (Foldable (..))
import           Data.Functor              ((<$>))

import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as Map
import           Data.Maybe                (fromMaybe, mapMaybe)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Prelude                   hiding (foldr, sequence)
import qualified Text.Parsec.Pos           as Pos

import qualified Inferno.Builtins          as Builtins
import           Inferno.InferState
import           Inferno.Log
import           Inferno.Pretty
import           Inferno.Types
import           Inferno.Unify             (unify, unifyAll, unifyl)


----------------------------------------------------------------------

-- var x = 2;    --> let x = ref 2 in    | x :: a
-- x = 3;        -->   x := 3            |

-- var f = function (x) { return [x]; }    --> let f = ref (\x -> arr [x])  :: Ref (forall a. a -> [a])
-- var g = f;                              -->     g = ref (!f)             :: Ref (forall a. a -> [a])
-- var st = f('abc');                      -->     st = ref (!f 'abc')      :: Ref [String]
-- var num = f(1234);                      -->     num = ref (!f 1234)      :: Ref [Number]

----------------------------------------------------------------------


-- instance (Functor f, Foldable f, Types a) => Types (f a) where
--   freeTypeVars = foldr (Set.union . freeTypeVars) Set.empty
--   applySubst s = fmap (applySubst s)
----------------------------------------------------------------------

getQuantificands :: TScheme -> [TVarName]
getQuantificands (TScheme tvars _) = tvars

getAnnotations :: Exp a -> [a]
getAnnotations = foldr (:) []


-- alphaEquivalent :: TScheme -> TScheme -> Bool
-- alphaEquivalent ts1@(TScheme tvn1 _) (TScheme tvn2 t2) = ts1 == TScheme tvn1 ts2'
--     where TScheme _ ts2' = applySubst substVarNames (TScheme [] t2)
--           substVarNames = Map.fromList . map (\(old,new) -> (old, TBody $ TVar new)) $ zip tvn2 tvn1

----------------------------------------------------------------------


isExpansive :: Exp a -> Bool
isExpansive (EVar _ _)        = False
isExpansive (EApp _ _ _)      = True
isExpansive (EAssign _ _ _ _) = True
isExpansive (EPropAssign _ _ _ _ _) = True
isExpansive (EIndexAssign _ _ _ _ _) = True
isExpansive (ELet _ _ _ _)    = True
isExpansive (EAbs _ _ _)      = False
isExpansive (ELit _ _)        = False
isExpansive (EArray _ _)  = True
isExpansive (ETuple _ _)  = True
isExpansive (ERow _ _ _)    = True
isExpansive (EIfThenElse _ e1 e2 e3) = any isExpansive [e1, e2, e3]
isExpansive (EProp _ e _)  = isExpansive e
isExpansive (EIndex _ a b)  = any isExpansive [a, b]
isExpansive (ENew _ _ _) = True
----------------------------------------------------------------------

closeRowList :: TRowList Type -> TRowList Type
closeRowList (TRowProp n t rest) = TRowProp n t (closeRowList rest)
closeRowList (TRowEnd _) = TRowEnd Nothing
-- TODO: Handle TRowRec, by defining a new named type in which all row types within are closed (recursively).

-- | Replaces a top-level open row type with the closed equivalent.
-- >>> closeRow (Fix $ TRow $ TRowProp "a" (Fix $ TRow $ TRowProp "a.a" (Fix $ TBody TNumber) (TRowEnd (Just $ RowTVar 1))) (TRowEnd (Just $ RowTVar 2)))
-- Fix (TRow (TRowProp "a" Fix (TRow (TRowProp "a.a" Fix (TBody TNumber) (TRowEnd (Just (RowTVar 1))))) (TRowEnd Nothing)))
-- >>> closeRow (Fix $ TCons TFunc [Fix $ TRow $ TRowProp "a" (Fix $ TRow $ TRowProp "a.a" (Fix $ TBody TNumber) (TRowEnd Nothing)) (TRowEnd Nothing), Fix $ TBody TString])
-- Fix (TCons TFunc [Fix (TRow (TRowProp "a" Fix (TRow (TRowProp "a.a" Fix (TBody TNumber) (TRowEnd Nothing))) (TRowEnd Nothing))),Fix (TBody TString)])
-- >>> closeRow (Fix $ TCons TFunc [Fix $ TRow $ TRowProp "a" (Fix $ TRow $ TRowProp "a.a" (Fix $ TBody TNumber) (TRowEnd (Just $ RowTVar 1))) (TRowEnd (Just $ RowTVar 2)), Fix $ TBody TString])
-- Fix (TCons TFunc [Fix (TRow (TRowProp "a" Fix (TRow (TRowProp "a.a" Fix (TBody TNumber) (TRowEnd (Just (RowTVar 1))))) (TRowEnd (Just (RowTVar 2))))),Fix (TBody TString)])
closeRow :: Type -> Type
closeRow (Fix (TRow r)) = Fix . TRow $ closeRowList r
closeRow t = t

----------------------------------------------------------------------

-- For efficiency reasons, types list is returned in reverse order.
accumInfer :: TypeEnv -> [Exp Pos.SourcePos] -> Infer [(Type, Exp (Pos.SourcePos, Type))]
accumInfer env =
  do traceLog ("accumInfer: env: " ++ pretty env) ()
     foldM accumInfer' []
     where accumInfer' types expr =
             do (t, e) <- inferType env expr
                return ((t,e):types)

inferType  :: TypeEnv -> Exp Pos.SourcePos -> Infer (Type, Exp (Pos.SourcePos, Type))
inferType env expr = do
  traceLog (">> " ++ pretty expr) ()
  (t, e) <- inferType' env expr
  s <- getMainSubst
  return (applySubst s t, fmap (applySubst s) e)

inferType' :: TypeEnv -> Exp Pos.SourcePos -> Infer (Type, Exp (Pos.SourcePos, Type))
inferType' _ (ELit a lit) = do
  let t = Fix $ TBody $ case lit of
                    LitNumber _ -> TNumber
                    LitBoolean _ -> TBoolean
                    LitString _ -> TString
                    LitRegex _ _ _ -> TRegex
                    LitUndefined -> TUndefined
                    LitNull -> TNull
  return (t, ELit (a,t) lit)
inferType' env (EVar a n) =
  do t <- instantiateVar a n env
     return (t, EVar (a, t) n)
inferType' env (EAbs a argNames e2) =
  do argTypes <- forM argNames (const $ Fix . TBody . TVar <$> fresh)
     env' <- foldM (\e (n, t) -> addVarScheme e n $ TScheme [] t) env $ zip argNames argTypes
     (t1, e2') <- inferType env' e2
     let t = Fix $ TCons TFunc $ argTypes ++ [t1]
     return (t, EAbs (a, t) argNames e2')
inferType' env (EApp a e1 eArgs) =
  do tvar <- Fix . TBody . TVar <$> fresh
     (t1, e1') <- inferType env e1
     argsTE <- tracePretty "EApp: unify type args" <$> accumInfer env eArgs
     let rargsTE = reverse argsTE
         tArgs = map fst rargsTE
         eArgs' = map snd rargsTE
     unify a t1 (Fix . TCons TFunc $ tArgs ++ [tvar])
     return (tvar, EApp (a, tvar) e1' eArgs')
inferType' env (ENew a e1 eArgs) =
  do (t1, e1') <- inferType env e1
     argsTE <- accumInfer env eArgs
     thisT <- Fix . TBody . TVar <$> fresh
     resT <- Fix . TBody . TVar <$> fresh
     let rargsTE = reverse argsTE
         tArgs = thisT : map fst rargsTE
         eArgs' = map snd rargsTE
     unify a t1 (Fix . TCons TFunc $ tArgs ++ [resT])
     resolvedThisT <- applyMainSubst thisT -- otherwise closeRow will not do what we want.
     unify a thisT (closeRow resolvedThisT)
     return (thisT, ENew (a, thisT) e1' eArgs')
inferType' env (ELet a n e1 e2) =
  do recType <- Fix . TBody . TVar <$> fresh
     recEnv <- addVarScheme env n $ TScheme [] recType
     (t1, e1') <- inferType recEnv e1
     unify a t1 recType
     let generalizeScheme = tracePretty ("let generalized '" ++ pretty n ++ "' --") <$> generalize env t1
     t' <- if isExpansive e1
           then return $ TScheme [] t1
           else generalizeScheme
     env' <- addVarScheme env n t'
     (t2, e2') <- inferType env' e2
     return (t2, ELet (a, t2) n e1' e2')
-- | Handling of mutable variable assignment.
-- | Prevent mutable variables from being polymorphic.
inferType' env (EAssign a n expr1 expr2) =
  do lvalueScheme <- getVarScheme a n env `failWithM` throwError a ("Unbound variable: " ++ show n ++ " in assignment " ++ pretty expr1)
     lvalueT <- instantiate lvalueScheme
     (rvalueT, expr1') <- inferType env expr1
     unify a rvalueT lvalueT
     unifyAllInstances a $ getQuantificands lvalueScheme
     (tRest, expr2') <- inferType env expr2
     return (tRest, EAssign (a, tRest) n expr1' expr2')
inferType' env (EPropAssign a objExpr n expr1 expr2) =
  do (objT, objExpr') <- inferType env objExpr
     (rvalueT, expr1') <- inferType env expr1
     rowTailVar <- RowTVar <$> fresh
     unify a objT $ Fix . TRow $ TRowProp n rvalueT $ TRowEnd (Just rowTailVar)
     unifyAllInstances a [getRowTVar rowTailVar]
     (expr2T, expr2') <- inferType env expr2
     return (expr2T, EPropAssign (a, expr2T) objExpr' n expr1' expr2')
inferType' env (EIndexAssign a eArr eIdx expr1 expr2) =
  do (tArr, eArr') <- inferType env eArr
     elemTVarName <- fresh
     let elemType = Fix . TBody . TVar $ elemTVarName
     unify a (Fix $ TCons TArray [elemType]) tArr
     (tId, eIdx') <- inferType env eIdx
     unify a (Fix $ TBody TNumber) tId
     (tExpr1, expr1') <- inferType env expr1
     unify a tExpr1 elemType
     unifyAllInstances a [elemTVarName]
     (tExpr2, expr2') <- inferType env expr2
     return (tExpr2 , EIndexAssign (a, tExpr2)  eArr' eIdx' expr1' expr2')
inferType' env (EArray a exprs) =
  do tv <- Fix . TBody . TVar <$> fresh
     te <- accumInfer env exprs
     let types = map fst te
     unifyl unify a $ zip (tv:types) types
     let t = Fix $ TCons TArray [tv]
     return (t, EArray (a,t) $ map snd te)
inferType' env (ETuple a exprs) =
  do te <- accumInfer env exprs
     let t = Fix . TCons TTuple . reverse $ map fst te
     return (t, ETuple (a,t) $ map snd te)
inferType' env (ERow a isOpen propExprs) =
  do te <- accumInfer env $ map snd propExprs
     endVar <- RowTVar <$> fresh
     let propNamesTypes = zip (map fst propExprs) (reverse $ map fst te)
         rowEnd' = TRowEnd $ if isOpen then Just endVar else Nothing
         rowType = Fix . TRow $ foldr (\(n,t') r -> TRowProp n t' r) rowEnd' propNamesTypes
     return (rowType, ERow (a,rowType) isOpen $ zip (map fst propExprs) (map snd te))
inferType' env (EIfThenElse a ePred eThen eElse) =
  do (tp, ePred') <- inferType env ePred
     unify a (Fix $ TBody TBoolean) tp
     (tThen, eThen') <- inferType env eThen
     (tElse, eElse') <- inferType env eElse
     unify a tThen tElse
     return (tThen, EIfThenElse (a, tThen) ePred' eThen' eElse')
inferType' env (EProp a eObj propName) =
  do (tObj, eObj') <- inferType env eObj
     rowVar <- RowTVar <$> fresh
     propType <- Fix . TBody . TVar <$> fresh
     unify a tObj $ Fix . TRow $ TRowProp propName propType $ TRowEnd (Just rowVar)
     return (propType, EProp (a,propType) eObj' propName)
inferType' env (EIndex a eArr eIdx) =
  do (tArr, eArr') <- inferType env eArr
     elemType <- Fix . TBody . TVar <$> fresh
     unify a (Fix $ TCons TArray [elemType]) tArr
     (tId, eIdx') <- inferType env eIdx
     unify a (Fix $ TBody TNumber) tId
     return (elemType, EIndex (a, elemType)  eArr' eIdx')

unifyAllInstances :: Pos.SourcePos -> [TVarName] -> Infer ()
unifyAllInstances a tvs = do
  m <- getVarInstances
  let equivalenceSets = mapMaybe (`Map.lookup` m) tvs

  -- TODO suboptimal - some of the sets may be identical
  let unifyAll' equivs = unifyAll a . tracePretty "equivalence:" $ Set.toList equivs
  mapM_ unifyAll' equivalenceSets

createEnv :: Map EVarName TScheme -> Infer (Map EVarName VarId)
createEnv builtins = foldM addVarScheme' Map.empty $ Map.toList builtins
    where allTVars :: TScheme -> Set TVarName
          allTVars (TScheme qvars t) = freeTypeVars t `Set.union` (Set.fromList qvars)

          safeLookup :: Eq a => [(a,a)] -> a -> a
          safeLookup assoc n = fromMaybe n $ lookup n assoc

          addVarScheme' :: Map EVarName VarId -> (EVarName, TScheme) -> Infer (Map EVarName VarId)
          addVarScheme' m (name, tscheme) =
            do allocNames <- forM (Set.toList $ allTVars tscheme) $ \tvName -> (fresh >>= return . (tvName,))
               addVarScheme m name $ mapVarNames (safeLookup allocNames) tscheme


typeInference :: Map EVarName TScheme -> Exp Pos.SourcePos -> Infer (Exp (Pos.SourcePos, Type))
typeInference builtins e =
  do env <- createEnv builtins
     (_t, e') <- inferType env e
     return e'

----------------------------------------------------------------------
--
-- | Mutable variable being assigned incompatible types:
--
-- >>> let p = Pos.initialPos "<dummy>"
-- >>> let fun = EAbs p
-- >>> let var = EVar p
-- >>> let let' = ELet p
-- >>> let tuple = ETuple p
-- >>> let app a b = EApp p a [b]
-- >>> let lit = ELit p
-- >>> let assign = EAssign p
-- >>> let array = EArray p
--
-- x is known to have type forall a. a -> a, and to have been used in a context requiring bool -> bool (e.g. `x True`)
--
-- we now try to assign x := \y -> 2
--
-- This should fail because it "collapses" x to be Number -> Number which is not compatible with bool -> bool
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (assign "x" (fun ["y"] (lit (LitNumber 0))) (tuple [var "x", var "y"])))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- The following should succeed because x is immutable and thus polymorphic:
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (tuple [var "x", var "y"]))
-- "((this: b -> b), (TNumber, TBoolean))"
--
-- The following should fail because x is mutable and therefore a monotype:
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (assign "x" (fun ["z1"] (var "z1")) (tuple [var "x", var "y"])))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- The following should also succeed because "x" is only ever used like this: (x True). The second assignment to x is: x := \z1 -> False, which is specific but matches the usage. Note that x's type is collapsed to: Boolean -> Boolean.
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (app (var "x") (lit (LitBoolean True))) (assign "x" (fun ["z1"] (lit (LitBoolean False))) (tuple [var "x", var "y"])))
-- "((this: TBoolean -> TBoolean), TBoolean)"
--
-- | Tests a setter for x being called with something more specific than x's original definition:
-- >>> :{
-- >>> test $ let'
-- >>> "x" (fun ["a"] (var "a"))
-- >>> (let' "setX"
-- >>>    (fun ["v"]
-- >>>             (let'
-- >>>          "_" (assign "x" (var "v") (var "x")) (lit (LitBoolean False))))
-- >>>    (let'
-- >>>       "_" (app (var "setX") (fun ["a"] (lit (LitString "a"))))
-- >>>       (app (var "x") (lit (LitBoolean True)))))
-- >>> :}
-- "<dummy>:1:1: Error: Could not unify: TString with TBoolean"
--
-- >>> test $ tuple [lit (LitBoolean True), lit (LitNumber 2)]
-- "(TBoolean, TNumber)"
--
-- >>> test $ let' "id" (fun ["x"] (var "x")) (assign "id" (fun ["y"] (var "y")) (var "id"))
-- "(this: a -> a)"
--
-- >>> test $ let' "id" (fun ["x"] (var "x")) (assign "id" (lit (LitBoolean True)) (var "id"))
-- "<dummy>:1:1: Error: Could not unify: TBoolean with (this: a -> a)"
--
-- >>> test $ let' "x" (lit (LitBoolean True)) (assign "x" (lit (LitBoolean False)) (var "x"))
-- "TBoolean"
--
-- >>> test $ let' "x" (lit (LitBoolean True)) (assign "x" (lit (LitNumber 3)) (var "x"))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- >>> test $ let' "x" (array [lit (LitBoolean True)]) (var "x")
-- "[TBoolean]"
--
-- >>> test $ let' "x" (array [lit $ LitBoolean True, lit $ LitBoolean False]) (var "x")
-- "[TBoolean]"
--
-- >>> test $ let' "x" (array []) (assign "x" (array []) (var "x"))
-- "[a]"
--
-- >>> test $ let' "x" (array [lit $ LitBoolean True, lit $ LitNumber 2]) (var "x")
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- >>> test $ let' "id" (fun ["x"] (let' "y" (var "x") (var "y"))) (app (var "id") (var "id"))
-- "(this: b -> b)"
--
-- >>> test $ let' "id" (fun ["x"] (let' "y" (var "x") (var "y"))) (app (app (var "id") (var "id")) (lit (LitNumber 2)))
-- "TNumber"
--
-- >>> test $ let' "id" (fun ["x"] (app (var "x") (var "x"))) (var "id")
-- "<dummy>:1:1: Error: Occurs check failed: a in (this: a -> b)"
--
-- >>> test $ fun ["m"] (let' "y" (var "m") (let' "x" (app (var "y") (lit (LitBoolean True))) (var "x")))
-- "(this: (this: TBoolean -> a) -> a)"
--
-- >>> test $ app (lit (LitNumber 2)) (lit (LitNumber 2))
-- "<dummy>:1:1: Error: Could not unify: TNumber with (this: TNumber -> a)"
--
-- EAssign tests
-- >>> test $ let' "x" (fun ["y"] (lit (LitNumber 0))) (assign "x" (fun ["y"] (var "y")) (var "x"))
-- "(this: TNumber -> TNumber)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (assign "x" (fun ["y"] (lit (LitNumber 0))) (var "x"))
-- "(this: TNumber -> TNumber)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))])
-- "(TNumber, TBoolean)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (app (var "x") (var "x"))
-- "(this: b -> b)"
--
-- >>> test $ let' "x" (fun ["a"] (var "a")) (let' "getX" (fun ["v"] (var "x")) (let' "setX" (fun ["v"] (let' "_" (assign "x" (var "v") (var "x")) (lit (LitBoolean True)))) (let' "_" (app (var "setX") (fun ["a"] (lit (LitString "a")))) (var "getX"))))
-- "(this: b -> (this: TString -> TString))"
test :: Exp Pos.SourcePos -> String
test e = case runTypeInference e of
          Left err -> pretty err
          Right expr -> pretty $ snd . head . getAnnotations . minifyVars $ expr


runTypeInference :: Exp Pos.SourcePos -> Either TypeError (Exp (Pos.SourcePos, Type))
runTypeInference e = runInfer $ typeInference Builtins.builtins e

