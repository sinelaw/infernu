--{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
--{-# LANGUAGE DeriveGeneric     #-}
--{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TemplateHaskell   #-} -- for quickcheck all
{-# LANGUAGE TupleSections     #-}

module Infer where


import           Control.Monad       (forM, forM_, foldM)
--import           Control.Monad.State (State, evalState, get, modify)
import           Data.Functor.Identity(Identity(..), runIdentity)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.State (StateT(..), evalStateT, get, modify) --, EitherT(..))
import           Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import           Data.Functor        ((<$>))
import           Data.List           (intercalate)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromMaybe, mapMaybe)
import qualified Data.Set            as Set
import           Data.Foldable       (Foldable(..))

import Prelude hiding (foldr)
    
-- import           Test.QuickCheck(choose)
--import           Test.QuickCheck.All    
-- import           Test.QuickCheck.Arbitrary(Arbitrary(..))
-- import           Data.DeriveTH

--import Debug.Trace(trace)
trace :: a -> b -> b
trace _ y = y

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

type EVarName = String

data LitVal = LitNumber Double | LitBoolean Bool | LitString String
            deriving (Show, Eq, Ord)

data Exp a = EVar a EVarName
           | EApp a (Exp a) (Exp a)
           | EAbs a EVarName (Exp a)
           | ELet a EVarName (Exp a) (Exp a)
           | ELit a LitVal
           | EAssign a EVarName (Exp a) (Exp a)
           | EArray a [Exp a]
           | ETuple a [Exp a]
             deriving (Show, Eq, Ord, Functor)

----------------------------------------------------------------------

type TVarName = Int

data TBody = TVar TVarName
            | TNumber | TBoolean | TString
            deriving (Show, Eq, Ord)

data TConsName = TFunc | TArray | TTuple
            deriving (Show, Eq, Ord)
               
data Type t = TBody t
            | TCons TConsName [Type t]
            deriving (Show, Eq, Ord, Functor)--, Foldable, Traversable)


type TSubst = Map.Map TVarName (Type TBody)


----------------------------------------------------------------------

class Types a where
  freeTypeVars :: a -> Set.Set TVarName
  applySubst :: TSubst -> a -> a

-- for convenience only:
instance Types a => Types (Maybe a) where
  freeTypeVars = maybe Set.empty freeTypeVars
  applySubst s = fmap $ applySubst s
      
instance Types a => Types [a] where
  freeTypeVars = Set.unions . map freeTypeVars
  applySubst s = map (applySubst s)

instance (Ord a, Types a) => Types (Set.Set a) where
  freeTypeVars = Set.foldr Set.union Set.empty . Set.map freeTypeVars
  applySubst s = Set.map (applySubst s)
                   
instance Types a => Types (Map.Map b a) where
  freeTypeVars m = freeTypeVars . Map.elems $ m
  applySubst s = Map.map (applySubst s)
  
----------------------------------------------------------------------

instance Types (Type TBody) where
  freeTypeVars (TBody (TVar n)) = Set.singleton n
  freeTypeVars (TBody _) = Set.empty
  freeTypeVars (TCons _ ts) = Set.unions $ map freeTypeVars ts

  applySubst s t@(TBody (TVar n)) = fromMaybe t $ Map.lookup n s
  applySubst _ t@(TBody _) = t
  applySubst s (TCons n ts) = TCons n (applySubst s ts)


-- instance (Functor f, Foldable f, Types a) => Types (f a) where
--   freeTypeVars = foldr (Set.union . freeTypeVars) Set.empty
--   applySubst s = fmap (applySubst s)

                              
----------------------------------------------------------------------

-- | Type scheme: a type expression with a "forall" over some type variables that may appear in it (universal quantification).
data TScheme = TScheme [TVarName] (Type TBody)
             deriving (Show, Eq)

instance Types TScheme where
  freeTypeVars (TScheme qvars t) = freeTypeVars t `Set.difference` Set.fromList qvars
  -- | When subsituting on a TScheme, we allow replacing quantified vars! 
  -- (i.e. we don't do (foldr Map.delete s qvars) $ applySubst t)
  applySubst s (TScheme qvars t) = TScheme qvars $ applySubst s t 

ungeneralize :: TScheme -> TScheme
ungeneralize (TScheme _ tbody) = TScheme [] tbody

getQuantificands :: TScheme -> [TVarName]
getQuantificands (TScheme tvars _) = tvars

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

prop_composeSubst :: TSubst -> TSubst -> Type TBody -> Bool
prop_composeSubst new old t = applySubst (composeSubst new old) t == applySubst new (applySubst old t)

----------------------------------------------------------------------

type VarId = TVarName
                     
-- | Type environment: maps AST variables (not type variables!) to quantified type schemes.
--
-- Note: instance of Types 
type TypeEnv = Map.Map EVarName VarId


getVarId :: EVarName -> TypeEnv -> Maybe VarId
getVarId = Map.lookup
                 
-- Used internally to generate fresh type variable names
data NameSource = NameSource { lastName :: TVarName }
                deriving (Show, Eq)

----------------------------------------------------------------------
-- | Adds an element to the value set of a map: key -> [value]
--
-- >>> let m1 = addToMappedSet 1 2 Map.empty
-- >>> m1
-- fromList [(1,fromList [2])]
-- >>> addToMappedSet 1 3 m1
-- fromList [(1,fromList [2,3])]
addToMappedSet :: (Ord a, Ord b) => a -> b -> Map.Map a (Set.Set b) -> Map.Map a (Set.Set b)
addToMappedSet k v m = Map.insert k (Set.insert v vs) m
    where vs = fromMaybe Set.empty $ Map.lookup k m

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


data InferState = InferState { nameSource :: NameSource
                             -- must be stateful because we sometimes discover that a variable is mutable.
                             , varSchemes :: Map.Map VarId TScheme
                             , varInstances :: Map.Map TVarName (Set.Set (Type TBody))
                             , mutableVars :: Set.Set VarId }
                  deriving (Show, Eq)

instance Types InferState where
    freeTypeVars = freeTypeVars . varSchemes
    applySubst s is = is { varSchemes = applySubst s (varSchemes is) }
                      
-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = StateT InferState (EitherT String Identity) a

runInferWith :: InferState -> Infer a -> Either String a
runInferWith ns inf = runIdentity . runEitherT $ evalStateT inf ns

runInfer :: Infer a -> Either String a
runInfer = runInferWith InferState { nameSource = NameSource { lastName = 0 }, varInstances = Map.empty, varSchemes = Map.empty, mutableVars = Set.empty }
                           
fresh :: Infer TVarName
fresh = do
  modify $ \is -> is { nameSource = (nameSource is) { lastName = lastName (nameSource is) + 1 } }
  lastName . nameSource <$> get

throwError :: String -> Infer a
throwError = lift . left

failWith :: Maybe a -> Infer a -> Infer a
failWith action err = case action of
                          Nothing -> err
                          Just x -> return x

failWithM :: Infer (Maybe a) -> Infer a -> Infer a
failWithM action err = do
  result <- action
  failWith result err

addMutableVar :: VarId -> Infer ()
addMutableVar varId = modify $ \is -> is { mutableVars = Set.insert varId $ mutableVars is }

getVarSchemeByVarId :: VarId -> Infer (Maybe TScheme)
getVarSchemeByVarId varId = Map.lookup varId . varSchemes <$> get

getVarScheme :: EVarName -> TypeEnv -> Infer (Maybe TScheme)
getVarScheme n env = case getVarId n env of
                       Nothing -> throwError $ "Unbound variable: '" ++ show n ++ "'"
                       Just varId -> getVarSchemeByVarId varId

setVarScheme :: EVarName -> VarId -> TScheme -> Infer ()
setVarScheme n varId scheme = do
  modify $ \is -> is { varSchemes = trace ("Inserting scheme for " ++ show n ++ ": " ++ pretty scheme) . Map.insert varId scheme $ varSchemes is }
  return ()

addVarScheme :: EVarName -> TypeEnv -> TScheme -> Infer TypeEnv
addVarScheme n env scheme = do
  varId <- trace' ("-- '" ++ show n ++ "' = varId") <$> fresh
  setVarScheme n varId scheme
  return $ Map.insert n varId env

addVarInstance :: TVarName -> TVarName -> Infer ()
addVarInstance x y = modify $ \is -> is { varInstances = trace' "updated equivs" $ addEquivalence x y (varInstances is) }

getEquivalences :: TVarName -> Infer (Set.Set (Type TBody))
getEquivalences n = fromMaybe Set.empty . Map.lookup n . varInstances <$> get

getFreeTVars :: TypeEnv -> Infer (Set.Set TVarName)                                                
getFreeTVars env = do
  mutableVarIds <- mutableVars <$> get
  let collectFreeTVs s varId = Set.union s <$> curFreeTVs 
          where curFreeTVs = maybe Set.empty freeTypeVars <$> getVarSchemeByVarId varId 
  foldM collectFreeTVs Set.empty (Map.elems env ++ Set.toList mutableVarIds)

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
--     instantiateVar "x" tenv'
-- :}
-- Right (TCons TFunc [TBody (TVar 2),TBody (TVar 1)])
--
instantiate :: TScheme -> Infer (Type TBody)
instantiate (TScheme tvarNames t) = do
  allocNames <- forM tvarNames $ \tvName -> do
    freshName <- fresh
    return (tvName, freshName)

  forM_ allocNames $ uncurry addVarInstance

  let replaceVar (TVar n) = TVar . fromMaybe n $ lookup n allocNames
      replaceVar x = x

  return $ fmap replaceVar t

instantiateVar :: EVarName -> TypeEnv -> Infer (Type TBody)
instantiateVar n env = do
  varId <- getVarId n env `failWith` throwError ("Unbound variable: '" ++ show n ++ "'")
  scheme <- getVarSchemeByVarId varId `failWithM` throwError ("Assertion failed: missing var scheme for: '" ++ show n ++ "'")
  trace' ("Instantiated var '" ++ show n ++ "' with scheme: " ++ pretty scheme ++ " to") <$> instantiate scheme

----------------------------------------------------------------------
-- | Generalizes a type to a type scheme, i.e. wraps it in a "forall" that quantifies over all
--   type variables that are free in the given type, but are not free in the type environment.
--
-- Example:
--
-- >>> runInfer $ generalize Map.empty $ TCons TFunc [TBody (TVar 0),TBody (TVar 1)]
-- Right (TScheme [0,1] (TCons TFunc [TBody (TVar 0),TBody (TVar 1)]))
--
-- >>> let t = TScheme [0] (TCons TFunc [TBody (TVar 0), TBody (TVar 1)])
-- >>> let tenv = Map.insert "x" t Map.empty
-- >>> tenv
-- fromList [("x",TScheme [0] (TCons TFunc [TBody (TVar 0),TBody (TVar 1)]))]
-- >>> runInfer $ generalize tenv (TCons TFunc [TBody (TVar 1), TBody (TVar 2)])
-- Right (TScheme [2] (TCons TFunc [TBody (TVar 1),TBody (TVar 2)]))
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

unify :: Type TBody -> Type TBody -> Infer TSubst
unify t1 t2 = trace' ("unify: \t" ++ show t1 ++ " ,\t " ++ show t2 ++ "\n\t-->") <$> unify' t1 t2
               
unify' :: Type TBody -> Type TBody -> Infer TSubst
unify' (TBody (TVar n)) t = varBind n t
unify' t (TBody (TVar n)) = varBind n t
unify' (TBody x) (TBody y) = if x == y
                             then return nullSubst
                             else throwError $ "Could not unify: " ++ pretty x ++ " with " ++ pretty y
unify' t1@(TBody _) t2@(TCons _ _) = throwError $ "Could not unify: " ++ pretty t1 ++ " with " ++ pretty t2
unify' t1@(TCons _ _) t2@(TBody _) = unify' t2 t1
unify' (TCons n1 ts1) (TCons n2 ts2) =
    if (n1 == n2) && (length ts1 == length ts2)
    then unifyl nullSubst $ zip ts1 ts2
    else throwError $ "TCons names or number of parameters do not match: " ++ pretty n1 ++ " /= " ++ pretty n2

-- | Unifies pairs of types, accumulating the substs
unifyl :: TSubst -> [(Type TBody, Type TBody)] -> Infer TSubst
unifyl = foldM unifyl'
    where unifyl' s (x, y) = do
            s' <- unify' (applySubst s x) (applySubst s y)
            return $ s' `composeSubst` s


varBind :: TVarName -> Type TBody -> Infer TSubst
varBind n t | t == TBody (TVar n) = return nullSubst
            | n `Set.member` freeTypeVars t = throwError $ "Occurs check failed: " ++ pretty n ++ " in " ++ pretty t
            | otherwise = return $ singletonSubst n t


-- | Drops the last element of a list. Does not entail an O(n) price.
-- >>> dropLast [1,2,3]
-- [1,2]
dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs

unifyAll :: TSubst -> [Type TBody] -> Infer TSubst
unifyAll s ts = unifyl s $ zip (dropLast ts) (drop 1 ts)

                          
----------------------------------------------------------------------

-- For efficiency reasons, types list is returned in reverse order.
accumInfer :: TypeEnv -> [Exp a] -> Infer (TSubst, [Type TBody])
accumInfer env = foldM accumInfer' (nullSubst, [])
    where accumInfer' (subst, types) expr = do
            (subst', t) <- inferType env expr
            applySubstInfer subst'
            return (subst' `composeSubst` subst, t:types)

inferType  :: TypeEnv -> Exp a -> Infer (TSubst, Type TBody)
inferType env expr = do
  (s,t) <- inferType' env expr
  state <- get
  return . trace (">> " ++ pretty expr ++ " :: " ++ pretty t ++ "\n\t" ++ show state ++ "\n\t Environment: " ++ show env ++ "\n----------") $ (s,t)

inferType' :: TypeEnv -> Exp a -> Infer (TSubst, Type TBody)
inferType' _ (ELit _ lit) = return . (nullSubst,) $ TBody $ case lit of
  LitNumber _ -> TNumber
  LitBoolean _ -> TBoolean
  LitString _ -> TString
inferType' env (EVar _ n) = do
  t <- instantiateVar n env
  return (nullSubst, t)
inferType' env (EAbs _ argName e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
     env' <- addVarScheme argName env $ TScheme [] tvar
     (s1, t1) <- inferType env' e2
     return (s1, TCons TFunc [applySubst s1 tvar, t1])
inferType' env (EApp _ e1 e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
     (s1, t1) <- inferType env e1
     applySubstInfer s1
     (s2, t2) <- inferType env e2
     applySubstInfer s2
     s3 <- trace' "\\ unified app" <$> unify (applySubst s2 t1) (TCons TFunc [t2, tvar])
     applySubstInfer s3
     return (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tvar)
inferType' env (ELet _ n e1 e2) =
  do (s1, t1) <- inferType env e1
     applySubstInfer s1
     t' <- trace' ("let generalized '" ++ show n ++ "' --") <$> generalize env t1
     env' <- addVarScheme n env t'
     (s2, t2) <- inferType env' e2
     return (s2 `composeSubst` s1, applySubst s2 t2)
-- | Handling of mutable variable assignment.
-- | Prevent mutable variables from being polymorphic.
inferType' env (EAssign _ n expr1 expr2) =
  do varId <- getVarId n env `failWith` throwError ("Assertion failed, missing varId for var: '" ++ show n ++ "'")
     addMutableVar varId
     (s1, rvalueT) <- inferType env expr1
     -- TODO use something like hoistEither (but for Maybe)
     lvalueScheme <- getVarScheme n env `failWithM` throwError ("Unbound variable: " ++ n ++ " in assignment " ++ pretty expr1)
     let ungeneralizedScheme = ungeneralize lvalueScheme 
     lvalueT <- instantiate ungeneralizedScheme
     setVarScheme n varId ungeneralizedScheme
     s2 <- unify rvalueT (applySubst s1 lvalueT)
     let s3 = s2 `composeSubst` s1
     s4 <- unifyAllInstances s3 $ getQuantificands lvalueScheme
     applySubstInfer s4
     (s5, tRest) <- inferType env expr2
     return (s5 `composeSubst` s4, tRest)
                      
inferType' env (EArray _ exprs) =
  do tvName <- fresh
     let tv = TBody $ TVar tvName
     (subst, types) <- accumInfer env exprs
     subst' <- unifyl subst $ zip (tv:types) types
     return (subst', TCons TArray [applySubst subst' $ TBody $ TVar tvName])
inferType' env (ETuple _ exprs) =
  do (subst, types) <- accumInfer env exprs
     return (subst, TCons TTuple $ reverse types)

unifyAllInstances :: TSubst -> [TVarName] -> Infer TSubst
unifyAllInstances s tvs = do
  m <- varInstances <$> get
  let equivalenceSets = mapMaybe (`Map.lookup` m) tvs

  -- TODO suboptimal - some of the sets may be identical
  let unifyAll' s' equivs = unifyAll s' . trace' "equivalence:" $ Set.toList equivs
  trace' "unified equivs:" <$> foldM unifyAll' s equivalenceSets

typeInference :: TypeEnv -> Exp a -> Infer (Type TBody)
typeInference env e = do
  (s, t) <- inferType env e 
  return $ applySubst s t

----------------------------------------------------------------------


tab :: Int -> String
tab t = replicate (t*4) ' '

class Pretty a where
  prettyTab :: Int -> a -> String

pretty :: Pretty a => a -> String
pretty = prettyTab 0

instance Pretty LitVal where
  prettyTab _ (LitNumber x) = show x
  prettyTab _ (LitBoolean x) = show x
  prettyTab _ (LitString x) = show x

instance Pretty EVarName where
  prettyTab _ x = x

instance Pretty (Exp a) where
  prettyTab t (EVar _ n) = prettyTab t n
  prettyTab t (EApp _ e1 e2) = prettyTab t e1 ++ " " ++ prettyTab t e2
  prettyTab t (EAbs _ n e) = "(\\" ++ prettyTab t n ++ " -> " ++ prettyTab t e ++ ")"
  prettyTab t (ELet _ n e1 e2) = "let " ++ prettyTab t n ++ " = " ++ prettyTab (t+1) e1 ++ "\n" ++ tab t ++ " in " ++ prettyTab (t+1) e2
  prettyTab t (ELit _ l) = prettyTab t l
  prettyTab t (EAssign _ n e1 e2) = prettyTab t n ++ " := " ++ prettyTab t e1 ++ ";\n" ++ tab t ++ prettyTab t e2
  prettyTab t (EArray _ es) = "[" ++ intercalate ", " (map (prettyTab t) es) ++ "]"
  prettyTab t (ETuple _ es) = "(" ++ intercalate ", " (map (prettyTab t) es) ++ ")"
                       
instance Pretty TVarName where
  prettyTab _ = show

instance Pretty TBody where
  prettyTab t (TVar n) = prettyTab t n
  prettyTab _ x = show x

instance Pretty TConsName where
  prettyTab _ = show
            
instance Pretty t => Pretty (Type t) where
  prettyTab n (TBody t) = prettyTab n t
  prettyTab n (TCons TFunc [t1, t2]) = "(" ++ prettyTab n t1 ++ " -> " ++ prettyTab n t2 ++ ")"
  prettyTab n (TCons TArray [t]) = "[" ++ prettyTab n t ++ "]"
  prettyTab n (TCons TTuple ts) = "(" ++ intercalate ", " (map (prettyTab n) ts) ++ ")"
  prettyTab _ _ = error "Unknown type for pretty"
             
instance Pretty TScheme where
  prettyTab n (TScheme vars t) = forall ++ prettyTab n t
      where forall = if null vars then "" else "forall " ++ unwords (map (prettyTab n) vars) ++ ". "

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    prettyTab n (Left x) = "Error: " ++ prettyTab n x
    prettyTab n (Right x) = prettyTab n x

----------------------------------------------------------------------
--
-- | Mutable variable being assigned incompatible types:
-- 
-- x is known to have type forall a. a -> a, and to have been used in a context requiring bool -> bool (e.g. `x True`)
--
-- we now try to assign x := \y -> 2
--
-- This should fail because it "collapses" x to be Number -> Number which is not compatible with bool -> bool
--
-- >>> test $ ELet "x" (EAbs "z" (EVar "z")) (ELet "y" (ETuple [EApp (EVar "x") (ELit (LitNumber 2)), EApp (EVar "x") (ELit (LitBoolean True))]) (EAssign "x" (EAbs "y" (ELit (LitNumber 0))) (ETuple [EVar "x", EVar "y"])))
-- "Error: Could not unify: TBoolean with TNumber"
--
-- The following should succeed because x is immutable and thus polymorphic:
--
-- >>> test $ ELet "x" (EAbs "z" (EVar "z")) (ELet "y" (ETuple [EApp (EVar "x") (ELit (LitNumber 2)), EApp (EVar "x") (ELit (LitBoolean True))]) (ETuple [EVar "x", EVar "y"]))
-- "((8 -> 8), (TNumber, TBoolean))"
--
-- The following should fail because x is mutable and therefore a monotype:
--
-- >>> test $ ELet "x" (EAbs "z" (EVar "z")) (ELet "y" (ETuple [EApp (EVar "x") (ELit (LitNumber 2)), EApp (EVar "x") (ELit (LitBoolean True))]) (EAssign "x" (EAbs "z1" (EVar "z1")) (ETuple [EVar "x", EVar "y"])))
-- "Error: Could not unify: TBoolean with TNumber"
--

-- |
-- The following should also succeed because "x" is only ever used like this: (x True). The second assignment to x is: x := \z1 -> False, which is specific but matches the usage. Note that x's type is collapsed to: Boolean -> Boolean.
--
-- >>> test $ ELet "x" (EAbs "z" (EVar "z")) (ELet "y" (EApp (EVar "x") (ELit (LitBoolean True))) (EAssign "x" (EAbs "z1" (ELit (LitBoolean False))) (ETuple [EVar "x", EVar "y"])))
-- "((TBoolean -> TBoolean), TBoolean)"
--

-- | Tests a setter for x being called with something more specific than x's original definition:
-- >>> :{
-- >>> test $ ELet
-- >>> "x" (EAbs "a" (EVar "a"))
-- >>> (ELet "setX"
-- >>>    (EAbs "v"
-- >>>             (ELet
-- >>>          "_" (EAssign "x" (EVar "v") (EVar "x")) (ELit (LitBoolean False))))
-- >>>    (ELet
-- >>>       "_" (EApp (EVar "setX") (EAbs "a" (ELit (LitString "a"))))
-- >>>       (EApp (EVar "x") (ELit (LitBoolean True)))))
-- >>> :}
-- "Error: Could not unify: TString with TBoolean"

-- | 'test' is a utility function for running the following tests:
--
-- >>> test $ ETuple () [ELit () (LitBoolean True), ELit () (LitNumber 2)]
-- "(TBoolean, TNumber)"
--
-- >>> test $ ELet () "id" (EAbs () "x" (EVar () "x")) (EAssign () "id" (EAbs () "y" (EVar () "y")) (EVar () "id"))
-- "(1 -> 1)"
--
-- >>> test $ ELet () "id" (EAbs () "x" (EVar () "x")) (EAssign () "id" (ELit () (LitBoolean True)) (EVar () "id"))
-- "Error: Could not unify: TBoolean with (1 -> 1)"
--
-- >>> test $ ELet () "x" (ELit () (LitBoolean True)) (EAssign () "x" (ELit () (LitBoolean False)) (EVar () "x"))
-- "TBoolean"
--
-- >>> test $ ELet () "x" (ELit () (LitBoolean True)) (EAssign () "x" (ELit () (LitNumber 3)) (EVar () "x"))
-- "Error: Could not unify: TNumber with TBoolean"
--
-- >>> test $ ELet () "x" (EArray () [ELit () (LitBoolean True)]) (EVar () "x")
-- "[TBoolean]"
--
-- >>> test $ ELet () "x" (EArray () [ELit () $ LitBoolean True, ELit () $ LitBoolean False]) (EVar () "x")
-- "[TBoolean]"
--
-- >>> test $ ELet () "x" (EArray () []) (EAssign () "x" (EArray () []) (EVar () "x"))
-- "[1]"
--
-- >>> test $ ELet () "x" (EArray () [ELit () $ LitBoolean True, ELit () $ LitNumber 2]) (EVar () "x")
-- "Error: Could not unify: TNumber with TBoolean"
--
-- >>> test $ ELet () "id" (EAbs () "x" (ELet () "y" (EVar () "x") (EVar () "y"))) (EApp () (EVar () "id") (EVar () "id"))
-- "(7 -> 7)"
--
-- >>> test $ ELet () "id" (EAbs () "x" (ELet () "y" (EVar () "x") (EVar () "y"))) (EApp () (EApp () (EVar () "id") (EVar () "id")) (ELit () (LitNumber 2)))
-- "TNumber"
--
-- >>> test $ ELet () "id" (EAbs () "x" (EApp () (EVar () "x") (EVar () "x"))) (EVar () "id")
-- "Error: Occurs check failed: 1 in (1 -> 3)"
--
-- >>> test $ EAbs () "m" (ELet () "y" (EVar () "m") (ELet () "x" (EApp () (EVar () "y") (ELit () (LitBoolean True))) (EVar () "x")))
-- "((TBoolean -> 4) -> 4)"
--
-- >>> test $ EApp () (ELit () (LitNumber 2)) (ELit () (LitNumber 2))
-- "Error: Could not unify: TNumber with (TNumber -> 1)"

-- | EAssign
-- >>> test $ ELet () "x" (EAbs () "y" (ELit () (LitNumber 0))) (EAssign () "x" (EAbs () "y" (EVar () "y")) (EVar () "x"))
-- "(TNumber -> TNumber)"
--
-- >>> test $ ELet () "x" (EAbs () "y" (EVar () "y")) (EAssign () "x" (EAbs () "y" (ELit () (LitNumber 0))) (EVar () "x"))
-- "(TNumber -> TNumber)"
--
-- >>> test $ ELet () "x" (EAbs () "y" (EVar () "y")) (ETuple () [EApp () (EVar () "x") (ELit () (LitNumber 2)), EApp () (EVar () "x") (ELit () (LitBoolean True))])
-- "(TNumber, TBoolean)"
--
-- >>> test $ ELet () "x" (EAbs () "y" (EVar () "y")) (EApp () (EVar () "x") (EVar () "x"))
-- "(6 -> 6)"
--
-- >>> test $ ELet () "x" (EAbs () "a" (EVar () "a")) (ELet () "getX" (EAbs () "v" (EVar () "x")) (ELet () "setX" (EAbs () "v" (ELet () "_" (EAssign () "x" (EVar () "v") (EVar () "x")) (ELit () (LitBoolean True)))) (ELet () "_" (EApp () (EVar () "setX") (EAbs () "a" (ELit () (LitString "a")))) (EVar () "getX"))))
-- "(16 -> (TString -> TString))"
test :: Exp a -> String
test e = pretty . runInfer $ typeInference Map.empty e
         --in pretty e ++ " :: " ++ pretty t ++ "\n"
--     case res of
--       Left err -> putStrLn $ show e ++ "\n " ++ err ++ "\n"
--       Right t -> putStrLn $ show e ++ " :: " ++ show t ++ "\n"
    

-- Test runner

--return []

-- $( derive makeArbitrary ''TBody )
-- $( derive makeArbitrary ''Type )

--runAllTests = $(quickCheckAll)
       
