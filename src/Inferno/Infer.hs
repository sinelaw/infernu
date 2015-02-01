{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module Inferno.Infer
    ( runTypeInference
    , test
    , Pretty(..)
    , pretty
    , getAnnotations
    , minifyVars
    , TypeError
#ifdef QUICKCHECK
    , runAllTests
#endif
    )
    where


import           Control.Monad              (foldM, forM)
import           Data.Foldable              (Foldable (..))
import           Data.Functor               ((<$>))

import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)
import           Data.Maybe                 (fromMaybe, mapMaybe)
import qualified Data.Set                   as Set
import           Data.Set                   (Set)
import           Prelude                    hiding (foldr, sequence)
import qualified Text.Parsec.Pos            as Pos

#ifdef QUICKCHECK
import           Data.DeriveTH
import           Test.QuickCheck            (choose, resize)
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary  (Arbitrary (..))
#endif

import           Inferno.Unify ( unify, unifyAll, unifyl )

import           Inferno.Pretty
import           Inferno.Types
import qualified Inferno.Builtins            as Builtins

import           Inferno.InferState
import           Inferno.Log


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
----------------------------------------------------------------------
----------------------------------------------------------------------


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
accumInfer :: TSubst -> TypeEnv -> [Exp Pos.SourcePos] -> Infer (TSubst, [(Type, Exp (Pos.SourcePos, Type))])
accumInfer initialSubst env =
  do traceLog ("accumInfer: initialSubst: " ++ pretty initialSubst ++ ", env: " ++ pretty env) ()
     foldM accumInfer' (initialSubst, [])
     where accumInfer' (subst, types) expr =
             do (subst', t, e) <- inferType env expr
                applySubstInfer subst'
                return (subst' `composeSubst` subst, (applySubst subst t,e):types)

inferType  :: TypeEnv -> Exp Pos.SourcePos -> Infer (TSubst, Type, Exp (Pos.SourcePos, Type))
inferType env expr = do
  traceLog (">> " ++ pretty expr) ()
  (s, t, e) <- inferType' env expr
  return (s, t, e)

inferType' :: TypeEnv -> Exp Pos.SourcePos -> Infer (TSubst, Type, Exp (Pos.SourcePos, Type))
inferType' _ (ELit a lit) = do
  let t = Fix $ TBody $ case lit of
                    LitNumber _ -> TNumber
                    LitBoolean _ -> TBoolean
                    LitString _ -> TString
                    LitRegex _ _ _ -> TRegex
                    LitUndefined -> TUndefined
                    LitNull -> TNull
  return (nullSubst, t, ELit (a,t) lit)
inferType' env (EVar a n) = do
  t <- instantiateVar a n env
  return (nullSubst, t, EVar (a, t) n)
inferType' env (EAbs a argNames e2) =
  do argTypes <- forM argNames (const $ Fix . TBody . TVar <$> fresh)
     env' <- foldM (\e (n, t) -> addVarScheme e n $ TScheme [] t) env $ zip argNames argTypes
     (s1, t1, e2') <- inferType env' e2
     applySubstInfer s1
     let t = Fix $ TCons TFunc $ map (applySubst s1) argTypes ++ [t1]
     return (s1, t, EAbs (a, t) argNames e2')
inferType' env (EApp a e1 eArgs) =
  do tvarName <- fresh
     let tvar = Fix $ TBody (TVar tvarName)
     (s1, t1, e1') <- inferType env e1
     applySubstInfer s1
     (s2, argsTE) <- tracePretty "EApp: unify type args" <$> accumInfer s1 env eArgs
     applySubstInfer s2
     let rargsTE = reverse argsTE
         tArgs = map fst rargsTE
         eArgs' = map snd rargsTE
         s2' = s2 `composeSubst` s1
     s3 <- tracePretty "EApp: unify inferred with template" <$> unify a (applySubst s2' t1) (applySubst s2' $ Fix . TCons TFunc $ tArgs ++ [tvar])
     let s3' = s3 `composeSubst` s2'
         t = applySubst s3' tvar
     applySubstInfer s3'
     return (tracePretty "\\ unified app, subst: " $ s3', t, EApp (a, t) e1' eArgs')
inferType' env (ENew a e1 eArgs) =
  do (s1, t1, e1') <- inferType env e1
     applySubstInfer s1
     (s2, argsTE) <- accumInfer s1 env eArgs
     applySubstInfer s2
     thisTVarName <- fresh
     resT <- Fix . TBody . TVar <$> fresh
     let thisT = Fix . TBody $ TVar thisTVarName
         rargsTE = reverse argsTE
         tArgs = thisT : map fst rargsTE
         eArgs' = map snd rargsTE
         s2' = s2 `composeSubst` s1
     s3 <- tracePretty "ENew: unify inferred with template" <$> unify a (applySubst s2' t1) (applySubst s2' $ Fix . TCons TFunc $ tArgs ++ [resT])
     let s3' = s3 `composeSubst` s2'
         t = applySubst s3' thisT
     applySubstInfer s3'
     s4 <- unify a t (closeRow t)
     applySubstInfer s4
     let s4' = s4 `composeSubst` s3'
         t' = applySubst s4' t
     return (s4', t', ENew (a, t') e1' eArgs')
inferType' env (ELet a n e1 e2) =
  do recType <- Fix . TBody . TVar <$> fresh
     recEnv <- addVarScheme env n $ TScheme [] recType
     (s1, t1, e1') <- inferType recEnv e1
     applySubstInfer s1
     s1rec <- unify a t1 (applySubst s1 recType)
     applySubstInfer s1rec
     let s1' = s1rec `composeSubst` s1
         generalizeScheme = tracePretty ("let generalized '" ++ pretty n ++ "' --") <$> generalize env (applySubst s1' t1)
     t' <- if isExpansive e1
           then return $ TScheme [] $ applySubst s1' t1
           else generalizeScheme
     env' <- addVarScheme env n t'
     (s2, t2, e2') <- inferType env' e2
     let s2' = s2 `composeSubst` s1'
         t = applySubst s2' t2
     applySubstInfer s2'
     return (s2', t, ELet (a, t) n e1' e2')
-- | Handling of mutable variable assignment.
-- | Prevent mutable variables from being polymorphic.
inferType' env (EAssign a n expr1 expr2) =
  do lvalueScheme <- getVarScheme a n env `failWithM` throwError a ("Unbound variable: " ++ show n ++ " in assignment " ++ pretty expr1)
     lvalueT <- instantiate lvalueScheme
     (s1, rvalueT, expr1') <- inferType env expr1
     s2 <- unify a rvalueT (applySubst s1 lvalueT)
     let s3 = s2 `composeSubst` s1
     s4 <- unifyAllInstances a s3 $ getQuantificands lvalueScheme
     applySubstInfer s4
     (s5, tRest, expr2') <- inferType env expr2
     let tRest' = applySubst s4 tRest
         s6 = s5 `composeSubst` s4
     applySubstInfer s6
     return (s6, tRest', EAssign (a, tRest') n expr1' expr2')
inferType' env (EPropAssign a objExpr n expr1 expr2) =
  do (s1, objT, objExpr') <- inferType env objExpr
     applySubstInfer s1
     (s2, rvalueT, expr1') <- inferType env expr1
     applySubstInfer s2
     let s2' = s2 `composeSubst` s1
     rowTailVar <- RowTVar <$> fresh
     s3 <- unify a (applySubst s2' objT) $ applySubst s2' . Fix . TRow $ TRowProp n rvalueT $ TRowEnd (Just rowTailVar)
     applySubstInfer s3
     let s3' = s3 `composeSubst` s2'
     (s4, expr2T, expr2') <- inferType env expr2
     let s5 = s4 `composeSubst` s3'
     s6 <- unifyAllInstances a s5 [getRowTVar rowTailVar]
     let tRest = applySubst s6 expr2T
     return (s6, tRest, EPropAssign (a, tRest) objExpr' n expr1' expr2')
inferType' env (EIndexAssign a eArr eIdx expr1 expr2) =
  do (s1, tArr, eArr') <- inferType env eArr
     elemTVarName <- fresh
     let elemType = Fix . TBody . TVar $ elemTVarName
     s1' <- unify a (Fix $ TCons TArray [elemType]) tArr
     let s1'' = s1' `composeSubst` s1
     applySubstInfer s1''
     (s2, tId, eIdx') <- inferType env eIdx
     s2' <- unify a (Fix $ TBody TNumber) tId
     let s2'' = s2' `composeSubst` s2 `composeSubst` s1''
     applySubstInfer s2''
     let elemType' = applySubst s2'' elemType
     (s3, tExpr1, expr1') <- inferType env expr1
     s3' <- unify a tExpr1 elemType'
     let s3'' = s3' `composeSubst` s3 `composeSubst` s2''
     applySubstInfer s3''
     s3b <- unifyAllInstances a s3'' [elemTVarName]
     applySubstInfer s3b
     (s4, tExpr2, expr2') <- inferType env expr2
     let s4' = s4 `composeSubst` s3b
     applySubstInfer s4'
     let tRest = applySubst s4' tExpr2
     return (s4', tRest , EIndexAssign (a, tRest)  eArr' eIdx' expr1' expr2')
inferType' env (EArray a exprs) =
  do tvName <- fresh
     let tv = Fix . TBody $ TVar tvName
     (subst, te) <- accumInfer nullSubst env exprs
     let types = map fst te
     subst' <- unifyl unify a subst $ zip (tv:types) types
     applySubstInfer subst'
     let t = Fix $ TCons TArray [applySubst subst' $ Fix . TBody $ TVar tvName]
     return (subst', t, EArray (a,t) $ map snd te)
inferType' env (ETuple a exprs) =
  do (subst, te) <- accumInfer nullSubst env exprs
     let t = Fix . TCons TTuple . reverse $ map fst te
     return (subst, t, ETuple (a,t) $ map snd te)
inferType' env (ERow a isOpen propExprs) =
  do (s, te) <- accumInfer nullSubst env $ map snd propExprs
     applySubstInfer s
     endVar <- RowTVar <$> fresh
     let propNamesTypes = zip (map fst propExprs) (reverse $ map fst te)
         rowEnd' = TRowEnd $ if isOpen then Just endVar else Nothing
         rowType = Fix . TRow $ foldr (\(n,t') r -> TRowProp n t' r) rowEnd' propNamesTypes
         t = applySubst s rowType
     return (s, t, ERow (a,t) isOpen $ zip (map fst propExprs) (map snd te))
inferType' env (EIfThenElse a ePred eThen eElse) =
  do (s1, tp, ePred') <- inferType env ePred
     s2 <- unify a (Fix $ TBody TBoolean) tp
     let s3 = s2 `composeSubst` s1
     applySubstInfer s3
     (s4, tThen, eThen') <- inferType env eThen
     applySubstInfer s4
     (s5, tElse, eElse') <- inferType env eElse
     s6 <- unify a tThen tElse
     let s' = s6 `composeSubst` s5 `composeSubst` s4 `composeSubst` s3
     applySubstInfer s'
     return (s', tThen, EIfThenElse (a, tThen) ePred' eThen' eElse')
inferType' env (EProp a eObj propName) =
  do (s1, tObj, eObj') <- inferType env eObj
     rowVar <- RowTVar <$> fresh
     propVar <- fresh
     s2 <- unify a tObj $ Fix . TRow $ TRowProp propName (Fix . TBody $ TVar propVar) $ TRowEnd (Just rowVar)
     let s3 = s2 `composeSubst` s1
         t = applySubst s3 (Fix . TBody $ TVar propVar)
     applySubstInfer s3
     return (s3, t, EProp (a,t) eObj' propName)
inferType' env (EIndex a eArr eIdx) =
  do (s1, tArr, eArr') <- inferType env eArr
     elemType <- Fix . TBody . TVar <$> fresh
     s1' <- unify a (Fix $ TCons TArray [elemType]) tArr
     let s1'' = s1' `composeSubst` s1
     applySubstInfer s1''
     (s2, tId, eIdx') <- inferType env eIdx
     s2' <- unify a (Fix $ TBody TNumber) (applySubst s1'' tId)
     let s2'' = s2' `composeSubst` s2
     applySubstInfer s2''
     let elemType' = applySubst (s2'' `composeSubst` s1'') elemType
     return (s2'' `composeSubst` s1'', elemType' , EIndex (a, elemType')  eArr' eIdx')

unifyAllInstances :: Pos.SourcePos -> TSubst -> [TVarName] -> Infer TSubst
unifyAllInstances a s tvs = do
  m <- getVarInstances
  let equivalenceSets = mapMaybe (`Map.lookup` m) tvs

  -- TODO suboptimal - some of the sets may be identical
  let unifyAll' s' equivs = unifyAll a s' . tracePretty "equivalence:" $ Set.toList equivs
  tracePretty "unified equivs:" <$> foldM unifyAll' s equivalenceSets

createEnv :: Map EVarName TScheme -> Infer (Map EVarName VarId)
createEnv builtins = foldM addVarScheme' Map.empty $ Map.toList builtins
    where allTVars :: TScheme -> Set TVarName
          allTVars (TScheme qvars t) = freeTypeVars t `Set.union` (Set.fromList qvars)
          safeLookup :: Eq a => [(a,a)] -> a -> a
          safeLookup assoc n = fromMaybe n $ lookup n assoc
          addVarScheme' :: Map EVarName VarId -> (EVarName, TScheme) -> Infer (Map EVarName VarId)
          addVarScheme' m (name, tscheme) = do
            allocNames <- forM (Set.toList $ allTVars tscheme) $ \tvName -> (fresh >>= return . (tvName,))
            addVarScheme m name $ mapVarNames (safeLookup allocNames) tscheme


typeInference :: Map EVarName TScheme -> Exp Pos.SourcePos -> Infer (Exp (Pos.SourcePos, Type))
typeInference builtins e = do
  env <- createEnv builtins
  (_s, _t, e') <- inferType env e
  let e'' = (fmap . fmap) (applySubst _s) e'
  return e''

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


#ifdef QUICKCHECK

-- Test runner

return []


instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = Map.fromList <$> resize 2 arbitrary
    shrink m = map (flip Map.delete m) (Map.keys m)

$( derive makeArbitrary ''TRowList )
$( derive makeArbitrary ''TConsName )
$( derive makeArbitrary ''TBody )
$( derive makeArbitrary ''Type )

runAllTests :: IO Bool
runAllTests = $(quickCheckAll)

#endif
