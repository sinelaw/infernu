{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}

module SafeJS.Infer
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


import           Control.Monad              (foldM, forM, forM_)
--import           Control.Monad.State (State, evalState, get, modify)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..), left, runEitherT)
import           Control.Monad.Trans.State  (StateT (..), evalStateT, get,
                                             modify)
import           Data.Foldable              (Foldable (..))
import           Data.Functor               ((<$>))
import           Data.Functor.Identity      (Identity (..), runIdentity)
import qualified Data.Map.Lazy              as Map
import           Data.Maybe                 (fromMaybe, mapMaybe)
import qualified Data.Set                   as Set
import           Prelude                    hiding (foldr)
import qualified Text.Parsec.Pos            as Pos

#ifdef QUICKCHECK
import           Data.DeriveTH
import           Test.QuickCheck            (choose, resize)
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary  (Arbitrary (..))
#endif

import           SafeJS.Pretty
import           SafeJS.Types

#if TRACE
import           Debug.Trace                (trace)
#else
trace :: a -> b -> b
trace _ y = y
#endif

trace' :: Show a => String -> a -> a
trace' prefix x = trace (prefix ++ " " ++ show x) x

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

ungeneralize :: TScheme -> TScheme
ungeneralize (TScheme _ tbody) = TScheme [] tbody

getQuantificands :: TScheme -> [TVarName]
getQuantificands (TScheme tvars _) = tvars

getAnnotations :: Exp a -> [a]
getAnnotations = foldr (:) []


-- alphaEquivalent :: TScheme -> TScheme -> Bool
-- alphaEquivalent ts1@(TScheme tvn1 _) (TScheme tvn2 t2) = ts1 == TScheme tvn1 ts2'
--     where TScheme _ ts2' = applySubst substVarNames (TScheme [] t2)
--           substVarNames = Map.fromList . map (\(old,new) -> (old, TBody $ TVar new)) $ zip tvn2 tvn1

----------------------------------------------------------------------

nullSubst :: TSubst
nullSubst = Map.empty

-- | composeSubst should obey the law:
-- applySubst (composeSubst new old) t = applySubst new (applySubst old t)
composeSubst :: TSubst -> TSubst -> TSubst
composeSubst new old = applySubst new old `Map.union` new

singletonSubst :: TVarName -> Type TBody -> TSubst
singletonSubst = Map.singleton

#ifdef QUICKCHECK
prop_composeSubst :: TSubst -> TSubst -> Type TBody -> Bool
prop_composeSubst new old t = applySubst (composeSubst new old) t == applySubst new (applySubst old t)
#endif
----------------------------------------------------------------------

getVarId :: EVarName -> TypeEnv -> Maybe VarId
getVarId = Map.lookup

----------------------------------------------------------------------
-- | Adds a pair of equivalent items to an equivalence map.
--
-- >>> let m1 = addEquivalence 1 2 Map.empty
-- >>> m1
-- fromList [(1,fromList [TBody (TVar 1),TBody (TVar 2)]),(2,fromList [TBody (TVar 1),TBody (TVar 2)])]
--
addEquivalence :: TVarName -> TVarName -> Map.Map TVarName (Set.Set (Type TBody)) -> Map.Map TVarName (Set.Set (Type TBody))
addEquivalence x y m = Map.insert x updatedSet . Map.insert y updatedSet $ m
    where updatedSet = Set.insert (TBody $ TVar x) . Set.insert (TBody $ TVar y) $ Set.union (getSet x) (getSet y)
          getSet item = fromMaybe Set.empty $ Map.lookup item m


-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = StateT InferState (EitherT TypeError Identity) a

runInferWith :: InferState -> Infer a -> Either TypeError a
runInferWith ns inf = runIdentity . runEitherT $ evalStateT inf ns

runInfer :: Infer a -> Either TypeError a
runInfer = runInferWith InferState { nameSource = NameSource { lastName = 0 }, varInstances = Map.empty, varSchemes = Map.empty }

fresh :: Infer TVarName
fresh = do
  modify $ \is -> is { nameSource = (nameSource is) { lastName = lastName (nameSource is) + 1 } }
  lastName . nameSource <$> get

throwError :: Pos.SourcePos -> String -> Infer a
throwError p s = lift . left $ TypeError p s

failWith :: Maybe a -> Infer a -> Infer a
failWith action err = case action of
                          Nothing -> err
                          Just x -> return x

failWithM :: Infer (Maybe a) -> Infer a -> Infer a
failWithM action err = do
  result <- action
  failWith result err

getVarSchemeByVarId :: VarId -> Infer (Maybe TScheme)
getVarSchemeByVarId varId = Map.lookup varId . varSchemes <$> get

getVarScheme :: Pos.SourcePos -> EVarName -> TypeEnv -> Infer (Maybe TScheme)
getVarScheme a n env = case getVarId n env of
                       Nothing -> throwError a $ "Unbound variable: '" ++ show n ++ "'"
                       Just varId -> getVarSchemeByVarId varId

setVarScheme :: EVarName -> VarId -> TScheme -> Infer ()
setVarScheme n varId scheme = do
  modify $ \is -> is { varSchemes = trace ("Inserting scheme for " ++ show n ++ ": " ++ show scheme) . Map.insert varId scheme $ varSchemes is }
  return ()

addVarScheme :: EVarName -> TypeEnv -> TScheme -> Infer TypeEnv
addVarScheme n env scheme = do
  varId <- trace' ("-- '" ++ show n ++ "' = varId") <$> fresh
  setVarScheme n varId scheme
  return $ Map.insert n varId env

addVarInstance :: TVarName -> TVarName -> Infer ()
addVarInstance x y = modify $ \is -> is { varInstances = trace' "updated equivs" $ addEquivalence x y (varInstances is) }

getFreeTVars :: TypeEnv -> Infer (Set.Set TVarName)
getFreeTVars env = do
  let collectFreeTVs s varId = Set.union s <$> curFreeTVs
          where curFreeTVs = maybe Set.empty freeTypeVars <$> getVarSchemeByVarId varId
  foldM collectFreeTVs Set.empty (Map.elems env)

-- | Applies a subsitution onto the state (basically on the variable -> scheme map).
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [0] (TCons TFunc [TBody (TVar 0), TBody (TVar 1)])
--     let tenv = Map.empty
--     tenv' <- addVarScheme "x" tenv t
--     applySubstInfer $ Map.singleton 0 (TBody TString)
--     varSchemes <$> get
-- :}
-- Right (fromList [(1,TScheme [0] (TCons TFunc [TBody TString,TBody (TVar 1)]))])
--
applySubstInfer :: TSubst -> Infer ()
applySubstInfer s = modify $ \is -> is {
                      varSchemes = trace' ("Updated env map using subst: " ++ show s ++ " --> ") . applySubst s $ varSchemes is
                    , varInstances = applySubst s $ varInstances is
                    }

-- | Instantiate a type scheme by giving fresh names to all quantified type variables.
--
-- For example:
--
-- >>> runInferWith (InferState { nameSource = NameSource 2, varInstances = Map.empty, varSchemes = Map.empty }) . instantiate $ TScheme [0] (TCons TFunc [TBody (TVar 0), TBody (TVar 1)])
-- Right (TCons TFunc [TBody (TVar 3),TBody (TVar 1)])
--
-- In the above example, type variable 0 has been replaced with a fresh one (3), while the unqualified free type variable 1 has been left as-is.
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [0] (TCons TFunc [TBody (TVar 0), TBody (TVar 1)])
--     let tenv = Map.empty
--     tenv' <- addVarScheme "x" tenv t
--     instantiateVar (Pos.initialPos "") "x" tenv'
-- :}
-- Right (TCons TFunc [TBody (TVar 2),TBody (TVar 1)])
--
instantiate :: TScheme -> Infer (Type TBody)
instantiate (TScheme tvarNames t) = do
  allocNames <- forM tvarNames $ \tvName -> do
    freshName <- fresh
    return (tvName, freshName)

  forM_ allocNames $ uncurry addVarInstance

  let replaceVar n = fromMaybe n $ lookup n allocNames

  return $ mapVarNames replaceVar t

instantiateVar :: Pos.SourcePos -> EVarName -> TypeEnv -> Infer (Type TBody)
instantiateVar a n env = do
  varId <- getVarId n env `failWith` throwError a ("Unbound variable: '" ++ show n ++ "'")
  scheme <- getVarSchemeByVarId varId `failWithM` throwError a ("Assertion failed: missing var scheme for: '" ++ show n ++ "'")
  trace' ("Instantiated var '" ++ show n ++ "' with scheme: " ++ show scheme ++ " to") <$> instantiate scheme

----------------------------------------------------------------------
-- | Generalizes a type to a type scheme, i.e. wraps it in a "forall" that quantifies over all
--   type variables that are free in the given type, but are not free in the type environment.
--
-- Example:
--
-- >>> runInfer $ generalize Map.empty $ TCons TFunc [TBody (TVar 0),TBody (TVar 1)]
-- Right (TScheme [0,1] (TCons TFunc [TBody (TVar 0),TBody (TVar 1)]))
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [1] (TCons TFunc [TBody (TVar 0), TBody (TVar 1)])
--     tenv <- addVarScheme "x" Map.empty t
--     generalize tenv (TCons TFunc [TBody (TVar 0), TBody (TVar 2)])
-- :}
-- Right (TScheme [2] (TCons TFunc [TBody (TVar 0),TBody (TVar 2)]))
--
-- In this example the steps were:
--
-- 1. Environment: { x :: forall 0. 0 -> 1 }
--
-- 2. generalize (1 -> 2)
--
-- 3. result: forall 2. 1 -> 2
--
-- >>> runInfer $ generalize Map.empty (TCons TFunc [TBody (TVar 0), TBody (TVar 0)])
-- Right (TScheme [0] (TCons TFunc [TBody (TVar 0),TBody (TVar 0)]))
--
-- TODO add tests for monotypes
generalize :: TypeEnv -> Type TBody -> Infer TScheme
generalize tenv t = do
  unboundVars <- Set.difference (freeTypeVars t) <$> getFreeTVars tenv
  return $ TScheme (Set.toList unboundVars) t

----------------------------------------------------------------------

unify :: Pos.SourcePos -> Type TBody -> Type TBody -> Infer TSubst
unify a t1 t2 = trace' ("unify: \t" ++ show t1 ++ " ,\t " ++ show t2 ++ "\n\t-->") <$> unify' a t1 t2

unificationError :: (Pretty x, Pretty y) => Pos.SourcePos -> x -> y -> Infer b
unificationError pos x y = throwError pos $ "Could not unify: " ++ pretty x ++ " with " ++ pretty y

unify' :: Pos.SourcePos -> Type TBody -> Type TBody -> Infer TSubst
unify' a (TBody (TVar n)) t = varBind a n t
unify' a t (TBody (TVar n)) = varBind a n t
unify' a (TBody x) (TBody y) = if x == y
                             then return nullSubst
                             else unificationError a x y
unify' a t1@(TBody _) t2@(TCons _ _) = unificationError a t1 t2
unify' a t1@(TCons _ _) t2@(TBody _) = unify' a t2 t1
unify' a (TCons n1 ts1) (TCons n2 ts2) =
    if (n1 == n2) && (length ts1 == length ts2)
    then unifyl a nullSubst $ zip ts1 ts2
    else unificationError a n1 n2 --throwError $ "TCons names or number of parameters do not match: " ++ pretty n1 ++ " /= " ++ pretty n2
unify' a t1@(TRow _)    t2@(TCons _ _) = unificationError a t1 t2
unify' a t1@(TRow _)    t2@(TBody _)   = unificationError a t1 t2
unify' a t1@(TCons _ _) t2@(TRow _)    = unificationError a t1 t2
unify' a t1@(TBody _)   t2@(TRow _)    = unificationError a t1 t2
-- TODO: un-hackify!
unify' a t1@(TRow row1) t2@(TRow row2) =
    do let (m2, r2) = flattenRow row2
           names2 = Set.fromList $ Map.keys m2
           (m1, r1) = flattenRow row1
           names1 = Set.fromList $ Map.keys m1
           commonNames = Set.toList $ names1 `Set.intersection` names2
           namesToTypes :: Map.Map EPropName (Type a) -> [EPropName] -> [Type a]
           namesToTypes m = mapMaybe (flip Map.lookup m)
           commonTypes :: [(Type TBody, Type TBody)]
           commonTypes = zip (namesToTypes m1 commonNames) (namesToTypes m2 commonNames)
       s1 <- unifyl a nullSubst commonTypes
       r <- fresh
       s2 <- unifyRows a r s1 (t1, names1, m1) (t2, names2, r2)
       s3 <- unifyRows a r s2 (t2, names2, m2) (t1, names1, r1)
       return $ s3 `composeSubst` s2 `composeSubst` s1

unifyRows :: (Pretty y, Pretty x) => Pos.SourcePos -> TVarName -> TSubst
               -> (x, Set.Set EPropName, Map.Map EPropName (Type TBody))
               -> (y, Set.Set EPropName, Maybe TVarName)
               -> Infer TSubst
unifyRows a r s1 (t1, names1, m1) (t2, names2, r2) =
    do let in1NotIn2 = names1 `Set.difference` names2
           in1NotIn2row = unflattenRow m1 (Just r) (flip Set.member in1NotIn2)

       case r2 of
         Nothing -> if Set.null in1NotIn2
                    then return nullSubst
                    else unificationError a t1 t2
         Just r2' -> unify a (applySubst s1 $ TRow in1NotIn2row) (applySubst s1 $ TBody $ TVar r2')

-- | Unifies pairs of types, accumulating the substs
unifyl :: Pos.SourcePos -> TSubst -> [(Type TBody, Type TBody)] -> Infer TSubst
unifyl a = foldM unifyl'
    where unifyl' s (x, y) = do
            s' <- unify' a (applySubst s x) (applySubst s y)
            return $ s' `composeSubst` s


varBind :: Pos.SourcePos -> TVarName -> Type TBody -> Infer TSubst
varBind a n t | t == TBody (TVar n) = return nullSubst
              | n `Set.member` freeTypeVars t = throwError a $ "Occurs check failed: " ++ pretty n ++ " in " ++ pretty t
              | otherwise = return $ singletonSubst n t


-- | Drops the last element of a list. Does not entail an O(n) price.
-- >>> dropLast [1,2,3]
-- [1,2]
dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs

unifyAll :: Pos.SourcePos -> TSubst -> [Type TBody] -> Infer TSubst
unifyAll a s ts = unifyl a s $ zip (dropLast ts) (drop 1 ts)


isExpansive :: Exp a -> Bool
isExpansive (EVar _ _)        = True
isExpansive (EApp _ _ _)      = True
isExpansive (EAssign _ _ _ _) = True
isExpansive (EPropAssign _ _ _ _ _) = True
isExpansive (ELet _ _ _ _)    = True
isExpansive (EAbs _ _ _)      = False
isExpansive (ELit _ _)        = False
isExpansive (EArray _ exprs)  = any isExpansive exprs
isExpansive (ETuple _ exprs)  = any isExpansive exprs
isExpansive (ERow _ exprs)    = any isExpansive $ map snd exprs
isExpansive (EIfThenElse _ e1 e2 e3) = any isExpansive [e1, e2, e3]
isExpansive (EProp _ expr _)  = isExpansive expr

----------------------------------------------------------------------

-- For efficiency reasons, types list is returned in reverse order.
accumInfer :: TypeEnv -> [Exp Pos.SourcePos] -> Infer (TSubst, [(Type TBody, Exp (Pos.SourcePos, Type TBody))])
accumInfer env = foldM accumInfer' (nullSubst, [])
    where accumInfer' (subst, types) expr = do
            (subst', t, e) <- inferType env expr
            applySubstInfer subst'
            return (subst' `composeSubst` subst, (t,e):types)

inferType  :: TypeEnv -> Exp Pos.SourcePos -> Infer (TSubst, Type TBody, Exp (Pos.SourcePos, Type TBody))
inferType env expr = do
  (s, t, e) <- inferType' env expr
  state <- get
  let tr = trace $ ">> " ++ pretty expr ++ " :: " ++ pretty t ++ "\n\t" ++ show state ++ "\n\t Environment: " ++ show env ++ "\n----------"
  return . tr $ (s, t, e)

inferType' :: TypeEnv -> Exp Pos.SourcePos -> Infer (TSubst, Type TBody, Exp (Pos.SourcePos, Type TBody))
inferType' _ (ELit a lit) = do
  let t = TBody $ case lit of
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
inferType' env (EAbs a argName e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
     env' <- addVarScheme argName env $ TScheme [] tvar
     (s1, t1, e2') <- inferType env' e2
     let t = TCons TFunc [applySubst s1 tvar, t1]
     return (s1, t, EAbs (a, t) argName e2')
inferType' env (EApp a e1 e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
     (s1, t1, e1') <- inferType env e1
     applySubstInfer s1
     (s2, t2, e2') <- inferType env e2
     applySubstInfer s2
     s3 <- trace' "\\ unified app" <$> unify a (applySubst s2 t1) (TCons TFunc [t2, tvar])
     applySubstInfer s3
     let t = applySubst s3 tvar
     return (s3 `composeSubst` s2 `composeSubst` s1, t, EApp (a, t) e1' e2')
inferType' env (ELet a n e1 e2) =
  do recType <- TBody . TVar <$> fresh
     recEnv <- addVarScheme n env $ TScheme [] recType
     (s1, t1, e1') <- inferType recEnv e1
     s1rec <- unify a t1 recType
     let s1' = s1rec `composeSubst` s1
     applySubstInfer s1'
     let generalizeScheme = trace' ("let generalized '" ++ show n ++ "' --") <$> generalize env t1
     t' <- if isExpansive e1
           then return $ TScheme [] $ applySubst s1' t1
           else generalizeScheme
     env' <- addVarScheme n env t'
     (s2, t2, e2') <- inferType env' e2
     applySubstInfer s2
     let t = applySubst s2 t2
     return (s2 `composeSubst` s1', t, ELet (a, t) n e1' e2')
-- | Handling of mutable variable assignment.
-- | Prevent mutable variables from being polymorphic.
inferType' env (EAssign a n expr1 expr2) =
  do varId <- getVarId n env `failWith` throwError a ("Assertion failed, missing varId for var: '" ++ show n ++ "'")
     lvalueScheme <- getVarScheme a n env `failWithM` throwError a ("Unbound variable: " ++ show n ++ " in assignment " ++ pretty expr1)
     let ungeneralizedScheme = ungeneralize lvalueScheme
     lvalueT <- instantiate ungeneralizedScheme
     setVarScheme n varId ungeneralizedScheme
     (s1, rvalueT, expr1') <- inferType env expr1
     s2 <- unify a rvalueT (applySubst s1 lvalueT)
     let s3 = s2 `composeSubst` s1
     s4 <- unifyAllInstances a s3 $ getQuantificands lvalueScheme
     applySubstInfer s4
     (s5, tRest, expr2') <- inferType env expr2
     return (s5 `composeSubst` s4, tRest, EAssign (a, tRest) n expr1' expr2')
inferType' env (EPropAssign a objExpr n expr1 expr2) =
  do (s1, objT, objExpr') <- inferType env objExpr
     applySubstInfer s1
     (s2, rvalueT, expr1') <- inferType env expr1
     applySubstInfer s2
     rowVar <- fresh
     s3 <- unify a objT $ TRow $ TRowProp n rvalueT $ TRowEnd (Just rowVar)
     applySubstInfer s3
     (s4, expr2T, expr2') <- inferType env expr2
     let s5 = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
     return (s5, expr2T, EPropAssign (a,expr2T) objExpr' n expr1' expr2')
inferType' env (EArray a exprs) =
  do tvName <- fresh
     let tv = TBody $ TVar tvName
     (subst, te) <- accumInfer env exprs
     let types = map fst te
     subst' <- unifyl a subst $ zip (tv:types) types
     applySubstInfer subst'
     let t = TCons TArray [applySubst subst' $ TBody $ TVar tvName]
     return (subst', t, EArray (a,t) $ map snd te)
inferType' env (ETuple a exprs) =
  do (subst, te) <- accumInfer env exprs
     let t = TCons TTuple . reverse $ map fst te
     return (subst, t, ETuple (a,t) $ map snd te)
inferType' env (ERow a propExprs) =
  do (s, te) <- accumInfer env $ map snd propExprs
     applySubstInfer s
     let propNamesTypes = zip (map fst propExprs) (reverse $ map fst te)
         rowType = TRow $ foldr (\(n,t') r -> TRowProp n t' r) (TRowEnd Nothing) propNamesTypes
         t = applySubst s rowType
     return (s, t, ERow (a,t) $ zip (map fst propExprs) (map snd te))
inferType' env (EIfThenElse a ePred eThen eElse) =
  do (s1, tp, ePred') <- inferType env ePred
     s2 <- unify a (TBody TBoolean) tp
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
     rowVar <- fresh
     propVar <- fresh
     s2 <- unify a tObj $ TRow $ TRowProp propName (TBody $ TVar propVar) $ TRowEnd (Just rowVar)
     let s3 = s2 `composeSubst` s1
         t = applySubst s3 (TBody $ TVar propVar)
     applySubstInfer s3
     return (s3, t, EProp (a,t) eObj' propName)

unifyAllInstances :: Pos.SourcePos -> TSubst -> [TVarName] -> Infer TSubst
unifyAllInstances a s tvs = do
  m <- varInstances <$> get
  let equivalenceSets = mapMaybe (`Map.lookup` m) tvs

  -- TODO suboptimal - some of the sets may be identical
  let unifyAll' s' equivs = unifyAll a s' . trace' "equivalence:" $ Set.toList equivs
  trace' "unified equivs:" <$> foldM unifyAll' s equivalenceSets

minifyVars :: Type TBody -> Type TBody
minifyVars t = mapVarNames f t
    where vars = Map.fromList $ zip (Set.toList $ freeTypeVars t) ([1..] :: [TVarName])
          f n = fromMaybe n $ Map.lookup n vars

typeInference :: TypeEnv -> Exp Pos.SourcePos -> Infer (Exp (Pos.SourcePos, Type TBody))
typeInference env e = do
  (_s, _t, e') <- inferType env e
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
-- >>> let app = EApp p
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
-- >>> test $ let' "x" (fun "z" (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (assign "x" (fun "y" (lit (LitNumber 0))) (tuple [var "x", var "y"])))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- The following should succeed because x is immutable and thus polymorphic:
--
-- >>> test $ let' "x" (fun "z" (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (tuple [var "x", var "y"]))
-- "((a -> a), (TNumber, TBoolean))"
--
-- The following should fail because x is mutable and therefore a monotype:
--
-- >>> test $ let' "x" (fun "z" (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (assign "x" (fun "z1" (var "z1")) (tuple [var "x", var "y"])))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- The following should also succeed because "x" is only ever used like this: (x True). The second assignment to x is: x := \z1 -> False, which is specific but matches the usage. Note that x's type is collapsed to: Boolean -> Boolean.
--
-- >>> test $ let' "x" (fun "z" (var "z")) (let' "y" (app (var "x") (lit (LitBoolean True))) (assign "x" (fun "z1" (lit (LitBoolean False))) (tuple [var "x", var "y"])))
-- "((TBoolean -> TBoolean), TBoolean)"
--
-- Tests a setter for x being called with something more specific than x's original definition:
-- >>> :{
-- >>> test $ let'
-- >>> "x" (fun "a" (var "a"))
-- >>> (let' "setX"
-- >>>    (fun "v"
-- >>>             (let'
-- >>>          "_" (assign "x" (var "v") (var "x")) (lit (LitBoolean False))))
-- >>>    (let'
-- >>>       "_" (app (var "setX") (fun "a" (lit (LitString "a"))))
-- >>>       (app (var "x") (lit (LitBoolean True)))))
-- >>> :}
-- "<dummy>:1:1: Error: Could not unify: TString with TBoolean"
--
-- >>> test $ tuple [lit (LitBoolean True), lit (LitNumber 2)]
-- "(TBoolean, TNumber)"
--
-- >>> test $ let' "id" (fun "x" (var "x")) (assign "id" (fun "y" (var "y")) (var "id"))
-- "(a -> a)"
--
-- >>> test $ let' "id" (fun "x" (var "x")) (assign "id" (lit (LitBoolean True)) (var "id"))
-- "<dummy>:1:1: Error: Could not unify: TBoolean with (c -> c)"
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
-- >>> test $ let' "id" (fun "x" (let' "y" (var "x") (var "y"))) (app (var "id") (var "id"))
-- "(a -> a)"
--
-- >>> test $ let' "id" (fun "x" (let' "y" (var "x") (var "y"))) (app (app (var "id") (var "id")) (lit (LitNumber 2)))
-- "TNumber"
--
-- >>> test $ let' "id" (fun "x" (app (var "x") (var "x"))) (var "id")
-- "<dummy>:1:1: Error: Occurs check failed: c in (c -> e)"
--
-- >>> test $ fun "m" (let' "y" (var "m") (let' "x" (app (var "y") (lit (LitBoolean True))) (var "x")))
-- "((TBoolean -> a) -> a)"
--
-- >>> test $ app (lit (LitNumber 2)) (lit (LitNumber 2))
-- "<dummy>:1:1: Error: Could not unify: TNumber with (TNumber -> a)"
--
-- EAssign tests
-- >>> test $ let' "x" (fun "y" (lit (LitNumber 0))) (assign "x" (fun "y" (var "y")) (var "x"))
-- "(TNumber -> TNumber)"
--
-- >>> test $ let' "x" (fun "y" (var "y")) (assign "x" (fun "y" (lit (LitNumber 0))) (var "x"))
-- "(TNumber -> TNumber)"
--
-- >>> test $ let' "x" (fun "y" (var "y")) (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))])
-- "(TNumber, TBoolean)"
--
-- >>> test $ let' "x" (fun "y" (var "y")) (app (var "x") (var "x"))
-- "(a -> a)"
--
-- >>> test $ let' "x" (fun "a" (var "a")) (let' "getX" (fun "v" (var "x")) (let' "setX" (fun "v" (let' "_" (assign "x" (var "v") (var "x")) (lit (LitBoolean True)))) (let' "_" (app (var "setX") (fun "a" (lit (LitString "a")))) (var "getX"))))
-- "(a -> (TString -> TString))"
test :: Exp Pos.SourcePos -> String
test e = case runTypeInference e of
          Left err -> pretty err
          Right expr -> pretty . minifyVars . snd . head $ getAnnotations expr


runTypeInference :: Exp Pos.SourcePos -> Either TypeError (Exp (Pos.SourcePos, Type TBody))
runTypeInference e = runInfer $ typeInference Map.empty e


#ifdef QUICKCHECK

-- Test runner

return []


instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> resize 2 arbitrary
    shrink m = map (flip Map.delete m) (Map.keys m)

$( derive makeArbitrary ''TRowList )
$( derive makeArbitrary ''TConsName )
$( derive makeArbitrary ''TBody )
$( derive makeArbitrary ''Type )

runAllTests :: IO Bool
runAllTests = $(quickCheckAll)


#endif
