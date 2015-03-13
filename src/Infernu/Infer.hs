{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Infernu.Infer
    ( runTypeInference
    , test
    , Pretty(..)
    , pretty
    , getAnnotations
    , minifyVars
    , TypeError
    )
    where


import           Control.Monad      (foldM, forM)
import           Data.Foldable      (Foldable (..))
import           Data.Functor       ((<$>))
import           Data.Traversable   (mapM)

import           Data.Map.Lazy      (Map)
import qualified Data.Map.Lazy      as Map
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Prelude            hiding (foldr, mapM, sequence)

import           Data.List          (intercalate)

import qualified Infernu.Builtins   as Builtins
import           Infernu.InferState
import           Infernu.Lib        (safeLookup)
import           Infernu.Log
import           Infernu.Pretty
import           Infernu.Types
import           Infernu.Unify      (unify, unifyAll, unifyPending, unifyPredsL, unifyRowPropertyBiased, unifyl)



getQuantificands :: TypeScheme -> [TVarName]
getQuantificands (TScheme tvars _) = tvars

getAnnotations :: Exp a -> [a]
getAnnotations = foldr (:) []

----------------------------------------------------------------------

closeRowList :: TRowList Type -> TRowList Type
closeRowList (TRowProp n t rest) = TRowProp n t (closeRowList rest)
closeRowList (TRowEnd _) = TRowEnd Nothing
-- TODO: Handle TRowRec, by defining a new named type in which all row types within are closed (recursively).

-- | Replaces a top-level open row type with the closed equivalent.
-- >>> pretty $ closeRow (Fix $ TRow $ TRowProp "a" (schemeEmpty $ Fix $ TRow $ TRowProp "aa" (schemeEmpty $ Fix $ TBody TNumber) (TRowEnd (Just $ RowTVar 1))) (TRowEnd (Just $ RowTVar 2)))
-- "{a: {aa: Number, ..b}}"
-- >>> pretty $ closeRow (Fix $ TFunc [Fix $ TRow $ TRowProp "a" (schemeEmpty $ Fix $ TRow $ TRowProp "aa" (schemeEmpty $ Fix $ TBody TNumber) (TRowEnd Nothing)) (TRowEnd Nothing)] (Fix $ TBody TString))
-- "{a: {aa: Number}}.(() -> String)"
-- >>> pretty $ closeRow (Fix $ TFunc [Fix $ TRow $ TRowProp "a" (schemeEmpty $ Fix $ TRow $ TRowProp "a.a" (schemeEmpty $ Fix $ TBody TNumber) (TRowEnd (Just $ RowTVar 1))) (TRowEnd (Just $ RowTVar 2))] (Fix $ TBody TString))
-- "{a: {a.a: Number, ..b}, ..c}.(() -> String)"
closeRow :: Type -> Type
closeRow (Fix (TRow r)) = Fix . TRow $ closeRowList r
closeRow t = t

----------------------------------------------------------------------


-- For efficiency reasons, types list is returned in reverse order.
accumInfer :: TypeEnv -> [Exp Source] -> Infer [(QualType, Exp (Source, QualType))]
accumInfer env =
  do traceLog ("accumInfer: env: " ++ pretty env)
     foldM accumInfer' []
     where accumInfer' types expr =
             do (t, e) <- inferType env expr
                return ((t,e):types)

inferType  :: TypeEnv -> Exp Source -> Infer (QualType, Exp (Source, QualType))
inferType env expr = do
  traceLog (">> " ++ pretty expr ++ " -- env: " ++ pretty env)
  (t, e) <- inferType' env expr
  unifyPending
  s <- getMainSubst
  st <- getState
  traceLog (">> " ++ pretty expr ++ " -- inferred :: " ++ pretty (applySubst s t))
  traceLog ("   infer state: " ++ prettyTab 3 st)
  return (applySubst s t, fmap (applySubst s) e)

inferType' :: TypeEnv -> Exp Source -> Infer (QualType, Exp (Source, QualType))
inferType' _ (ELit a lit) = do
  let t = Fix $ TBody $ case lit of
                    LitNumber _ -> TNumber
                    LitBoolean _ -> TBoolean
                    LitString _ -> TString
                    LitRegex{} -> TRegex
                    LitUndefined -> TUndefined
                    LitNull -> TNull
  return (qualEmpty t, ELit (a, qualEmpty t) lit)
inferType' env (EVar a n) =
  do t <- instantiateVar a n env
     return (t, EVar (a, t) n)
inferType' env (EAbs a argNames e2) =
  do argTypes <- forM argNames (const $ Fix . TBody . TVar <$> fresh)
     env' <- foldM (\e (n, t) -> addVarScheme e n $ schemeEmpty t) env $ zip argNames argTypes
     (t1, e2') <- inferType env' e2
     pred' <- unifyPredsL a $ qualPred t1
     let t = TQual pred' $ Fix $ TFunc argTypes (qualType t1)
     return (t, EAbs (a, t) argNames e2')
inferType' env (EApp a e1 eArgs) =
  do tvar <- Fix . TBody . TVar <$> fresh
     (t1, e1') <- inferType env e1
     traceLog $ "EApp: Inferred type for func expr: " ++ pretty t1
     argsTE <- accumInfer env eArgs
     traceLog $ "EApp: Inferred types for func args: " ++ intercalate ", " (map pretty argsTE)
     let rargsTE = reverse argsTE
         tArgs = map fst rargsTE
         eArgs' = map snd rargsTE
         preds = concatMap qualPred $ t1:tArgs
     unify a (qualType t1) (Fix $ TFunc (map qualType tArgs) tvar)
     traceLog $ "Inferred preds: " ++ intercalate ", " (map pretty preds)
     tvar' <- do  pred' <- unifyPredsL a preds
                  tvarSubsted <- applyMainSubst tvar
                  return $ TQual pred' tvarSubsted
     traceLog ("Inferred func application: " ++ pretty tvar')
     return (tvar', EApp (a, tvar') e1' eArgs')
inferType' env (ENew a e1 eArgs) =
  do (t1, e1') <- inferType env e1
     argsTE <- accumInfer env eArgs
     thisT <- Fix . TBody . TVar <$> fresh
     resT <- Fix . TBody . TVar <$> fresh
     let rargsTE = reverse argsTE
         tArgs = thisT : map (qualType . fst) rargsTE
         eArgs' = map snd rargsTE
         preds = concatMap qualPred $ t1 : map fst argsTE
     unify a (qualType t1) (Fix $ TFunc tArgs resT)
     -- constrain 'this' to be a row type:
     rowConstraintVar <- RowTVar <$> fresh
     unify a (Fix . TRow . TRowEnd $ Just rowConstraintVar) thisT
     -- close the row type
     resolvedThisT <- applyMainSubst thisT -- otherwise closeRow will not do what we want.
     unify a thisT (closeRow resolvedThisT)
     -- TODO: If the function returns a row type, it should be the resulting type; other it should be 'thisT'
     preds' <- unifyPredsL a preds
     let thisT' = TQual preds' thisT
     return (thisT', ENew (a, thisT') e1' eArgs')
inferType' env (ELet a n e1 e2) =
  do recType <- Fix . TBody . TVar <$> fresh
     recEnv <- addVarScheme env n $ schemeEmpty recType
     (t1, e1') <- inferType recEnv e1
     unify a (qualType t1) recType
     t' <- generalize e1 env t1
     env' <- addVarScheme env n t'
     (t2, e2') <- inferType env' e2
     preds' <- unifyPredsL a $ concatMap qualPred [t1, t2]
     let resT = TQual preds' $ qualType t2
     return (resT, ELet (a, resT) n e1' e2')
-- | Handling of mutable variable assignment.
-- | Prevent mutable variables from being polymorphic.
inferType' env expr@(EAssign a n expr1 expr2) =
  do traceLog $ "EAssign: " ++ pretty expr
     lvalueScheme <- getVarScheme a n env `failWithM` throwError a ("Unbound variable: " ++ show n ++ " in assignment " ++ pretty expr1)
     traceLog $ "EAssign lvalueScheme: " ++ pretty lvalueScheme
     lvalueT <- instantiate lvalueScheme
     (rvalueT, expr1') <- inferType env expr1
     unify a (qualType lvalueT) (qualType rvalueT)
     (tRest, expr2') <- inferType env expr2
     traceLog $ "EAssign lvalueT: " ++ pretty lvalueT
     traceLog $ "EAssign Invoking unifyAllInstances on scheme: " ++ pretty lvalueScheme
     instancePreds <- unifyAllInstances a $ getQuantificands lvalueScheme
     preds <- unifyPredsL a $ concat $ (instancePreds:) $ map qualPred [lvalueT, rvalueT, tRest] -- TODO should update variable scheme
     -- update the variable scheme, removing perhaps some quantified tvars
     varId <- getVarId n env `failWith` throwError a ("Unbound variable: '" ++ show n ++ "'")
     updatedScheme <- generalize expr1 env (schemeType lvalueScheme)
     _ <- setVarScheme env n updatedScheme varId
     --
     let tRest' = TQual preds $ qualType tRest
     return (tRest', EAssign (a, tRest') n expr1' expr2')
inferType' env (EPropAssign a objExpr n expr1 expr2) =
  do (objT, objExpr') <- inferType env objExpr
     (rvalueT, expr1') <- inferType env expr1
     rowTailVar <- RowTVar <$> fresh
     let rvalueScheme = schemeFromQual rvalueT -- generalize expr1 env rvalueT
         rank0Unify = unify a (qualType objT) $ Fix . TRow $ TRowProp n rvalueScheme $ TRowEnd (Just rowTailVar)
     case unFix (qualType objT) of
       TRow trowList ->
         case Map.lookup n . fst $ flattenRow trowList of
          -- lvalue is known to be a property with some scheme
          Just lvalueScheme ->
            do generalizedRvalue <- generalize expr1 env rvalueT
               unifyRowPropertyBiased a rank0Unify (lvalueScheme, generalizedRvalue)
          Nothing -> rank0Unify
       _ -> rank0Unify
     (expr2T, expr2') <- inferType env expr2 -- TODO what about the pred
     traceLog "EPropAssign - applying unifyAllInstances"
     instancePred <- unifyAllInstances a [getRowTVar rowTailVar]
     preds <- unifyPredsL a $ concat $ (instancePred:) $ map qualPred [objT, rvalueT, expr2T] -- TODO review
     let tRes = TQual preds $ qualType expr2T
     return (tRes, EPropAssign (a, tRes) objExpr' n expr1' expr2')
inferType' env (EIndexAssign a eArr eIdx expr1 expr2) =
  do (tArr, eArr') <- inferType env eArr
     elemTVarName <- fresh
     arrTVarName <- fresh
     idxTVarName <- fresh
     let elemType = Fix . TBody . TVar $ elemTVarName
--     unify a (qualType tArr) $ Fix $ TCons TArray [elemType]
     unify a (qualType tArr) $ Fix $ TBody $ TVar arrTVarName
     (tId, eIdx') <- inferType env eIdx
     unify a (qualType tId) $ Fix $ TBody $ TVar idxTVarName
     (tExpr1, expr1') <- inferType env expr1
     unify a (qualType tExpr1) elemType
     -- TODO: BUG here, because elemTVarName never has any var instances due to the predicates usage here.
     --traceLog "EIndexAssign - applying unifyAllInstances"
     --instancePred <- unifyAllInstances a [elemTVarName]
     (tExpr2, expr2') <- inferType env expr2
     let curPred = indexAccessPred arrTVarName elemTVarName idxTVarName
     preds <- unifyPredsL a $ concat $ ([curPred]:) $ map qualPred [tArr, tId, tExpr1, tExpr2] -- TODO review
     let tRes = TQual preds $ qualType tExpr2
     return (tRes , EIndexAssign (a, tRes)  eArr' eIdx' expr1' expr2')
inferType' env (EArray a exprs) =
  do tv <- Fix . TBody . TVar <$> fresh
     te <- accumInfer env exprs
     let types = map (qualType . fst) te
     unifyl unify a $ zip (tv:types) types
     let t = qualEmpty $ Fix $ TCons TArray [tv]
     return (t, EArray (a,t) $ map snd te)
inferType' env (ETuple a exprs) =
  do te <- accumInfer env exprs
     let t = TQual (concatMap (qualPred . fst) te) $ Fix . TCons TTuple . reverse $ map (qualType . fst) te
     return (t, ETuple (a,t) $ map snd te)
inferType' env (EStringMap a exprs') =
  do let exprs = map snd exprs'
     elemType <- Fix . TBody . TVar <$> fresh
     te <- accumInfer env exprs
     let types = map (qualType . fst) te
     unifyl unify a $ zip (elemType:types) types
     let t = qualEmpty . Fix $ TCons TStringMap [elemType]
     return (t, EStringMap (a,t) $ zip (map fst exprs') (map snd te))
inferType' env (ERow a isOpen propExprs) =
  do te <- accumInfer env $ map snd propExprs
     endVar <- RowTVar <$> fresh
     let propNamesTypes = zip propExprs (reverse $ map fst te)
         rowEnd' = TRowEnd $ if isOpen then Just endVar else Nothing
         accumRowProp' row ((propName, propExpr), propType) =
           do ts <- generalize propExpr env propType
              return $ TRowProp propName ts row
     rowType <- qualEmpty . Fix . TRow <$> foldM  accumRowProp' rowEnd' propNamesTypes
     return (rowType, ERow (a,rowType) isOpen $ zip (map fst propExprs) (map snd te))
inferType' env (EIfThenElse a ePred eThen eElse) =
  do (tp, ePred') <- inferType env ePred
     unify a (Fix $ TBody TBoolean) (qualType tp)
     (tThen, eThen') <- inferType env eThen
     (tElse, eElse') <- inferType env eElse
     unify a (qualType tThen) (qualType tElse)
     return (tThen, EIfThenElse (a, tThen) ePred' eThen' eElse')
inferType' env (EProp a eObj propName) =
  do (tObj, eObj') <- inferType env eObj
     rowVar <- RowTVar <$> fresh
     propTypeScheme <- schemeEmpty . Fix . TBody . TVar <$> fresh
         --case unFix (qualType tObj) of
--                  TRow tRowList -> --TODO
--                  _ -> schemeEmpty . Fix . TBody . TVar <$> fresh
     unify a (Fix . TRow $ TRowProp propName propTypeScheme $ TRowEnd (Just rowVar)) (qualType tObj)
     propType <- instantiate propTypeScheme
     return (propType, EProp (a,propType) eObj' propName)
inferType' env (EIndex a eArr eIdx) =
  do (tArr, eArr') <- inferType env eArr
     elemTVarName <- fresh
     arrTVarName <- fresh
     idxTVarName <- fresh
     unify a (qualType tArr) (Fix $ TBody $ TVar arrTVarName)
     (tId, eIdx') <- inferType env eIdx
     unify a (qualType tId) (Fix $ TBody $ TVar idxTVarName)
     let elemType' = qualEmpty $ Fix $ TBody $ TVar elemTVarName
         curPred = indexAccessPred arrTVarName elemTVarName idxTVarName
     preds <- unifyPredsL a $ (curPred:) $ concatMap qualPred [tArr, tId] -- TODO review
     let tRes = TQual preds $ qualType elemType'
     return (tRes, EIndex (a, tRes)  eArr' eIdx')

indexAccessPred :: TVarName -> TVarName -> TVarName -> TPred Type
indexAccessPred arrTVarName elemTVarName idxTVarName =
    let elemType = mkv elemTVarName
        mkv = Fix . TBody . TVar
    in
     TPredIsIn (ClassName "Indexable") (Fix $ TCons TTuple
                                        [ mkv arrTVarName
                                        , mkv idxTVarName
                                        , mkv elemTVarName
                                        ])
                                          -- Fix $ TCons TArray [elemType])
    -- , TPredIsIn "Index"
    --  `mkAnd` TPredEq idxTVarName (Fix $ TBody TNumber))
    -- `mkOr` (TPredEq arrTVarName (Fix $ TCons TStringMap [elemType])
    --         `mkAnd` TPredEq idxTVarName (Fix $ TBody TString))

unifyAllInstances :: Source -> [TVarName] -> Infer [TPred Type]
unifyAllInstances a tvs = do
  m <- getVarInstances
  traceLog $ "unifyAllInstances: " ++ pretty a ++ " Unifying all instances of tvars: " ++ intercalate ", " (map pretty tvs)
  -- TODO suboptimal - some of the sets may be identical
  let equivalenceSets = Set.toList . Set.fromList $ mapMaybe (`Map.lookup` m) tvs
      unifyAll' equivs =
          do  let equivsL = Set.toList equivs
                  qequivsL = map qualType equivsL
              traceLog $ "unifyAllInstances - equivalence:" ++ pretty qequivsL
              unifyAll a qequivsL
              return $ concatMap qualPred equivsL
  pred' <- concat <$> mapM unifyAll' equivalenceSets
  unifyPredsL a pred'

createEnv :: Map EVarName TypeScheme -> Infer (Map EVarName VarId)
createEnv builtins = foldM addVarScheme' Map.empty $ Map.toList builtins
    where allTVars :: TypeScheme -> Set TVarName
          allTVars (TScheme qvars t) = freeTypeVars t `Set.union` Set.fromList qvars

          addVarScheme' :: Map EVarName VarId -> (EVarName, TypeScheme) -> Infer (Map EVarName VarId)
          addVarScheme' m (name, tscheme) =
            do allocNames <- forM (Set.toList $ allTVars tscheme)
                             $ \tvName -> (tvName,) <$> fresh
               addVarScheme m name $ mapVarNames (safeLookup allocNames) tscheme


typeInference :: Map EVarName TypeScheme -> Exp Source -> Infer (Exp (Source, QualType))
typeInference builtins e =
  do env <- createEnv builtins
     (_t, e') <- inferType env e
     return e'

----------------------------------------------------------------------
--
-- | Mutable variable being assigned incompatible types:
--
-- >>> let p = emptySource
-- >>> let fun args = EAbs p ("this":args)
-- >>> let var = EVar p
-- >>> let let' = ELet p
-- >>> let tuple = ETuple p
-- >>> let lit = ELit p
-- >>> let app a b = EApp p a [lit LitUndefined, b]
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
-- ":1:1*: Error: Could not unify: Number with Boolean"
--
-- The following should succeed because x is immutable and thus polymorphic:
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (tuple [var "x", var "y"]))
-- "(c.(d -> d), (Number, Boolean))"
--
-- The following should fail because x is mutable and therefore a monotype:
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (assign "x" (fun ["z1"] (var "z1")) (tuple [var "x", var "y"])))
-- ":1:1*: Error: Could not unify: Number with Boolean"
--
-- The following should also succeed because "x" is only ever used like this: (x True). The second assignment to x is: x := \z1 -> False, which is specific but matches the usage. Note that x's type is collapsed to: Boolean -> Boolean.
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (app (var "x") (lit (LitBoolean True))) (assign "x" (fun ["z1"] (lit (LitBoolean False))) (tuple [var "x", var "y"])))
-- "((Boolean -> Boolean), Boolean)"
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
-- ":1:1*: Error: Could not unify: String with Boolean"
--
-- >>> test $ tuple [lit (LitBoolean True), lit (LitNumber 2)]
-- "(Boolean, Number)"
--
-- >>> test $ let' "id" (fun ["x"] (var "x")) (assign "id" (fun ["y"] (var "y")) (var "id"))
-- "a.(b -> b)"
--
-- >>> test $ let' "id" (fun ["x"] (var "x")) (assign "id" (lit (LitBoolean True)) (var "id"))
-- ":1:1*: Error: Could not unify: a.(b -> b) with Boolean"
--
-- >>> test $ let' "x" (lit (LitBoolean True)) (assign "x" (lit (LitBoolean False)) (var "x"))
-- "Boolean"
--
-- >>> test $ let' "x" (lit (LitBoolean True)) (assign "x" (lit (LitNumber 3)) (var "x"))
-- ":1:1*: Error: Could not unify: Boolean with Number"
--
-- >>> test $ let' "x" (array [lit (LitBoolean True)]) (var "x")
-- "[Boolean]"
--
-- >>> test $ let' "x" (array [lit $ LitBoolean True, lit $ LitBoolean False]) (var "x")
-- "[Boolean]"
--
-- >>> test $ let' "x" (array []) (assign "x" (array []) (var "x"))
-- "[a]"
--
-- >>> test $ let' "x" (array [lit $ LitBoolean True, lit $ LitNumber 2]) (var "x")
-- ":1:1*: Error: Could not unify: Number with Boolean"
--
-- >>> test $ let' "id" (fun ["x"] (let' "y" (var "x") (var "y"))) (app (var "id") (var "id"))
-- "c.(d -> d)"
--
-- >>> test $ let' "id" (fun ["x"] (let' "y" (var "x") (var "y"))) (app (app (var "id") (var "id")) (lit (LitNumber 2)))
-- "Number"
--
-- >>> test $ let' "id" (fun ["x"] (app (var "x") (var "x"))) (var "id")
-- ":1:1*: Error: Occurs check failed: a in (a -> b)"
--
-- >>> test $ fun ["m"] (let' "y" (var "m") (let' "x" (app (var "y") (lit (LitBoolean True))) (var "x")))
-- "a.((Boolean -> b) -> b)"
--
-- >>> test $ app (lit (LitNumber 2)) (lit (LitNumber 2))
-- ":1:1*: Error: Could not unify: Number with (Number -> a)"
--
-- EAssign tests
-- >>> test $ let' "x" (fun ["y"] (lit (LitNumber 0))) (assign "x" (fun ["y"] (var "y")) (var "x"))
-- "a.(Number -> Number)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (assign "x" (fun ["y"] (lit (LitNumber 0))) (var "x"))
-- "a.(Number -> Number)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))])
-- "(Number, Boolean)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (app (var "x") (var "x"))
-- "c.(d -> d)"
--
-- >>> test $ let' "x" (fun ["a"] (var "a")) (let' "getX" (fun ["v"] (var "x")) (let' "setX" (fun ["v"] (let' "_" (assign "x" (var "v") (var "x")) (lit (LitBoolean True)))) (let' "_" (app (var "setX") (fun ["a"] (lit (LitString "a")))) (var "getX"))))
-- "e.(f -> d.(String -> String))"
test :: Exp Source -> String
test e = case runTypeInference e of
          Left err -> pretty err
          Right expr -> pretty $ snd . head . getAnnotations . minifyVars $ expr


runTypeInference :: Exp Source -> Either TypeError (Exp (Source, QualType))
runTypeInference e = runInfer $ typeInference Builtins.builtins e

