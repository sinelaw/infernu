{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-} -- for quickcheck all
{-# LANGUAGE TupleSections     #-}

module Infer where

import           Control.Monad       (forM, foldM, forM_)
--import           Control.Monad.State (State, evalState, get, modify)
import           Data.Functor.Identity(Identity(..), runIdentity)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.State (StateT(..), evalStateT, get, modify) --, EitherT(..))
import           Control.Monad.Trans.Either (EitherT(..), runEitherT, left)
import           Data.Functor        ((<$>))
import           Data.List           (intercalate)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set
    
-- import           Test.QuickCheck(choose)
--import           Test.QuickCheck.All    
-- import           Test.QuickCheck.Arbitrary(Arbitrary(..))
-- import           Data.DeriveTH
import Debug.Trace(traceShowId, trace)
    
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

data Exp = EVar EVarName
         | EApp Exp Exp
         | EAbs EVarName Exp
         | ELet EVarName Exp Exp
         | ELit LitVal
         | EAssign EVarName Exp Exp
         | EArray [Exp]
         | ETuple [Exp]
         deriving (Show, Eq, Ord)

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
                                     
----------------------------------------------------------------------

-- | Type scheme: a type expression with a "forall" over some type variables that may appear in it (universal quantification).
data TScheme = TScheme [TVarName] (Type TBody)
             deriving (Show, Eq)

instance Types TScheme where
  freeTypeVars (TScheme qvars t) = freeTypeVars t `Set.difference` Set.fromList qvars
  applySubst s (TScheme qvars t) = TScheme qvars $ applySubst (foldr Map.delete s qvars) t

alphaEquivalent :: TScheme -> TScheme -> Bool                                   
alphaEquivalent ts1@(TScheme tvn1 _) (TScheme tvn2 t2) = ts1 == TScheme tvn1 ts2'
    where TScheme _ ts2' = applySubst substVarNames (TScheme [] t2)
          substVarNames = Map.fromList . map (\(old,new) -> (old, TBody $ TVar new)) $ zip tvn2 tvn1
    
----------------------------------------------------------------------

type VarId = TVarName
    
data VarInfo = VarInfo { scheme :: TScheme, varId :: VarId }
               deriving (Show, Eq)
                        
singletonVarInfo :: VarId -> TScheme -> VarInfo
singletonVarInfo n s = VarInfo { scheme = s, varId = n }
                     
instance Types VarInfo where
    freeTypeVars v = freeTypeVars $ scheme v
    applySubst s v = v { scheme = applySubst s $ scheme v }
                     
-- | Type environment: maps AST variables (not type variables!) to quantified type schemes.
--
-- Note: instance of Types 
type TypeEnv = Map.Map EVarName VarInfo


getVarId :: EVarName -> TypeEnv -> Maybe VarId
getVarId n env = fmap varId $ Map.lookup n env
                 
-- Used internally to generate fresh type variable names
data NameSource = NameSource { lastName :: TVarName }
                deriving (Show, Eq)


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

-- | Adds an element to the value set of a map: key -> value set
--
-- >>> let m1 = addToMappedSet 1 2 (Map.empty)
-- >>> m1
-- fromList [(1,fromList [2])]
-- >>> addToMappedSet 1 3 m1
-- fromList [(1,fromList [2,3])]
addToMappedSet :: (Ord a, Ord b) => a -> b -> (Map.Map a (Set.Set b)) -> (Map.Map a (Set.Set b))
addToMappedSet k v m = Map.insert k (Set.insert v set) m
    where set = case Map.lookup k m of
                  Nothing -> Set.empty
                  Just vs -> vs 

data InferState = InferState { nameSource :: NameSource, varInstances :: Map.Map TVarName (Set.Set (Type TBody)) }
                  deriving (Show, Eq)

instance Types InferState where
    freeTypeVars = freeTypeVars . varInstances
    applySubst s is = is { varInstances = applySubst s (varInstances is) }
                      
-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = StateT InferState (EitherT String Identity) a

runInferWith :: InferState -> Infer a -> Either String a
runInferWith ns inf = runIdentity . runEitherT $ evalStateT inf ns

runInfer :: Infer a -> Either String a
runInfer = runInferWith InferState { nameSource = NameSource { lastName = 0 }, varInstances = Map.empty }

addInstance :: VarId -> Type TBody -> Infer ()
addInstance varId' t = modify (\is -> is { varInstances = addToMappedSet varId' t $ varInstances is })

getInstances :: TypeEnv -> EVarName -> Infer (Set.Set (Type TBody))
getInstances env n = do
  let varId' = getVarId n env
  case varId' of
    Nothing -> return Set.empty
    Just varId'' ->
        do res <- Map.lookup varId'' . varInstances <$> get
           case res of
             Nothing -> return Set.empty
             Just types -> return types

-- | Applies the given subst to the set of instances saved in the set; for convience, returns the same argument it got.
substInstances :: Infer TSubst -> Infer TSubst
substInstances inf = do s <- inf
                        substInstances' s
                        return s
                               
substInstances' :: TSubst -> Infer ()
substInstances' s = modify (applySubst s)
                           
fresh :: Infer TVarName
fresh = do
  modify (\is -> is { nameSource = (nameSource is) { lastName = lastName (nameSource is) + 1 } })
  lastName . nameSource <$> get

throwError :: String -> Infer a
throwError = lift . left

-- | Instantiate a type scheme by giving fresh names to all quantified type variables.
--
-- For example:
--
-- >>> runInferWith (InferState { nameSource = NameSource 2, varInstances = Map.empty }) . instantiate $ TScheme [0] (TCons TFunc [TBody (TVar 0), TBody (TVar 1)])
-- Right (TCons TFunc [TBody (TVar 3),TBody (TVar 1)])
--
-- In the above example, type variable 0 has been replaced with a fresh one (3), while the unqualified free type variable 1 has been left as-is.
--
instantiate :: TScheme -> Infer (Type TBody)
instantiate (TScheme tvarNames t) = do
  allocNames <- forM tvarNames $ \tvName -> do
    freshName <- fresh
    return (tvName, freshName)

  let replaceVar (TVar n) = TVar . fromMaybe n $ lookup n allocNames
      replaceVar x = x

  return $ fmap replaceVar t

----------------------------------------------------------------------
-- | Generalizes a type to a type scheme, i.e. wraps it in a "forall" that quantifies over all
--   type variables that are free in the given type, but are not free in the type environment.
--
-- Example:
--
-- >>> let t = TScheme [0] (TCons TFunc [TBody (TVar 0), TBody (TVar 1)])
-- >>> let tenv = Map.insert "x" (singletonVarInfo 2 t) Map.empty
-- >>> tenv
-- fromList [("x",VarInfo {scheme = TScheme [0] (TCons TFunc [TBody (TVar 0),TBody (TVar 1)]), varId = 2})]
-- >>> generalize tenv (TCons TFunc [TBody (TVar 1), TBody (TVar 2)])
-- TScheme [2] (TCons TFunc [TBody (TVar 1),TBody (TVar 2)])
--
-- In this example the steps were:
--
-- 1. Environment: { x :: forall 0. 0 -> 1 }
--
-- 2. generalize (1 -> 2)
--
-- 3. result: forall 2. 1 -> 2
--
-- >>> generalize Map.empty (TCons TFunc [TBody (TVar 0), TBody (TVar 0)])
-- TScheme [0] (TCons TFunc [TBody (TVar 0),TBody (TVar 0)])
--
generalize :: TypeEnv -> Type TBody -> TScheme
generalize tenv t = TScheme (Set.toList (freeTypeVars t `Set.difference` freeTypeVars tenv)) t

----------------------------------------------------------------------

unify :: Type TBody -> Type TBody -> Infer TSubst
unify (TBody (TVar n)) t = substInstances $ varBind n t
unify t (TBody (TVar n)) = substInstances $ varBind n t
unify (TBody x) (TBody y) = if x == y
                            then return nullSubst
                            else throwError $ "Could not unify: " ++ pretty x ++ " with " ++ pretty y
unify t1@(TBody _) t2@(TCons _ _) = throwError $ "Could not unify: " ++ pretty t1 ++ " with " ++ pretty t2
unify t1@(TCons _ _) t2@(TBody _) = substInstances $ unify t2 t1
unify (TCons n1 ts1) (TCons n2 ts2) =
    if (n1 == n2) && (length ts1 == length ts2)
    then unifyl nullSubst ts1 ts2
    else throwError $ "TCons names or number of parameters do not match: " ++ pretty n1 ++ " /= " ++ pretty n2

unifyl :: TSubst -> [Type TBody] -> [Type TBody] -> Infer TSubst
unifyl initialSubst xs ys = substInstances $ foldM unifyl' initialSubst $ zip xs ys
    where unifyl' s (x, y) = do
            s' <- unify (applySubst s x) (applySubst s y)
            return $ s' `composeSubst` s

varBind :: TVarName -> Type TBody -> Infer TSubst
varBind n t | t == TBody (TVar n) = return nullSubst
            | n `Set.member` freeTypeVars t = throwError $ "Occurs check failed: " ++ pretty n ++ " in " ++ pretty t
            | otherwise = return $ singletonSubst n t


                          
----------------------------------------------------------------------

-- For efficiency reasons, types list is returned in reverse order.
accumInfer :: TypeEnv -> [Exp] -> Infer (TSubst, TypeEnv, [Type TBody])
accumInfer env = foldM accumInfer' (nullSubst, env, [])
    where accumInfer' (subst, env', types) expr = do
            (subst', t) <- inferType env' expr
            return (subst' `composeSubst` subst, applySubst subst' env, t:types)

inferType :: TypeEnv -> Exp -> Infer (TSubst, Type TBody)
inferType _ (ELit lit) = return . (nullSubst,) $ TBody $ case lit of
  LitNumber _ -> TNumber
  LitBoolean _ -> TBoolean
  LitString _ -> TString
inferType env (EVar n) = case Map.lookup n env of
  Nothing -> throwError $ "Unbound variable: " ++ n
  Just varInfo -> do
    t <- instantiate . scheme $ varInfo
    addInstance (varId varInfo) t
    return (nullSubst, t)
inferType env (EAbs argName e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
         env' = Map.insert argName (singletonVarInfo tvarName $ TScheme [] tvar) env
     (s1, t1) <- inferType env' e2
     return (s1, TCons TFunc [applySubst s1 tvar, t1])
inferType env (EApp e1 e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
     (s1, t1) <- inferType env e1
     (s2, t2) <- inferType (applySubst s1 env) e2
     s3 <- unify (applySubst s2 t1) (TCons TFunc [t2, tvar])
     return (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tvar)
inferType env (ELet n e1 e2) =
  do (s1, t1) <- inferType env e1
     varId' <- fresh
     let t' = generalize (applySubst s1 env) t1
         env' = Map.insert n (singletonVarInfo varId' t') env
     (s2, t2) <- inferType env' e2
     return (s2 `composeSubst` s1, t2)
-- | Handling of mutable variable assignment.
-- We keep track of how the var's type schema looks by unifying schemas every assignment.
--
-- TODO: missing - on every instantiation (during EVar reads) keep track also of what specific types
-- were used for the var. Then, during EAssign make sure that the instantiated schema can unify with
-- all the specific types that were encountered - in case our schema has just collapsed into a
-- too-specific type.
--
-- Example:
--
-- > x := \y -> y; z = [x 1, x False]; x := \y -> 0
--
-- In the first assignment, x :: forall a. a -> a
--
-- Then, z is succesfully inferred to be of type [number, bool] because x is polymorphic.
--
-- Lastly, we discover that x must actually be more specific: forall a. a -> Int and thus, our usage
-- of "x" in "z" was wrong and a type error should be raised.
--
-- The "greatest common denominator" part is implemented as follows:
--
-- Every time an EVar expression is encountered, the type scheme for the variable is instantiated
-- and the resulting type is used for that occurance, and is also added to a list of "type schema
-- instances for this variable".
--
-- Then, when an EAssign is encountered, we ensure that all previously encountered instances can be
-- instantiated from the current assignment's generalized type. We do this by unifying with each of
-- the previously instantiated types.
--
inferType env (EAssign n e1 e2) =
  do (s1, t1) <- inferType env e1
     -- TODO use something like hoistEither (but for Maybe)
     case Map.lookup n env of
       Nothing -> throwError $ "Unbound variable: " ++ n ++ " in assignment " ++ pretty e1
       Just varInfo ->
           do t2 <- instantiate . scheme $ varInfo
              s2 <- unify t1 $ applySubst s1 t2
              existingInstances <- getInstances env n
              let s3 = s2 `composeSubst` s1
                  env' = applySubst s3 env
                  env'' = Map.insert n (varInfo { scheme = generalize env' $ applySubst s3 t1 }) env'
                  newTypeScheme = generalize env'' . applySubst s3 $ t1
              let unifyInstances = foldM unifyInstances' s3 $ Set.toList existingInstances
                      where unifyInstances' s i = do
                              tempType <- instantiate newTypeScheme
                              s' <- unify (applySubst s i) (applySubst s tempType)
                              return $ s' `composeSubst` s
              res <- unifyInstances 
              (s4, tRest) <- inferType env'' e2
              return $ (s4 `composeSubst` s3, tRest)
                      
inferType env (EArray exprs) =
  do tvName <- fresh
     let tv = TBody $ TVar tvName
     (subst, _, types) <- accumInfer env exprs
     subst' <- unifyl subst (tv:types) types
     return (subst', TCons TArray [applySubst subst' $ TBody $ TVar tvName])
inferType env (ETuple exprs) =
  do (subst, _, types) <- accumInfer env exprs
     return (subst, TCons TTuple $ reverse types)
    
typeInference :: TypeEnv -> Exp -> Infer (Type TBody)
typeInference env e = do
  (s, t) <- inferType env e
  return $ applySubst s t

----------------------------------------------------------------------

class Pretty a where
  pretty :: a -> String

instance Pretty LitVal where
  pretty (LitNumber x) = show x
  pretty (LitBoolean x) = show x
  pretty (LitString x) = show x

instance Pretty EVarName where
  pretty x = x

instance Pretty Exp where
  pretty (EVar n) = pretty n
  pretty (EApp e1 e2) = pretty e1 ++ " " ++ pretty e2
  pretty (EAbs n e) = "(\\" ++ pretty n ++ " -> " ++ pretty e ++ ")"
  pretty (ELet n e1 e2) = "(let " ++ pretty n ++ " = " ++ pretty e1 ++ " in " ++ pretty e2 ++ ")"
  pretty (ELit l) = pretty l
  pretty (EAssign n e1 e2) = pretty n ++ " := " ++ pretty e1 ++ "; " ++ pretty e2
  pretty (EArray es) = "[" ++ intercalate ", " (map pretty es) ++ "]"
  pretty (ETuple es) = "(" ++ intercalate ", " (map pretty es) ++ ")"
                       
instance Pretty TVarName where
  pretty = show

instance Pretty TBody where
  pretty (TVar n) = pretty n
  pretty x = show x

instance Pretty TConsName where
  pretty = show
            
instance Pretty t => Pretty (Type t) where
  pretty (TBody t) = pretty t
  pretty (TCons TFunc [t1, t2]) = pretty t1 ++ " -> " ++ pretty t2
  pretty (TCons TArray [t]) = "[" ++ pretty t ++ "]"
  pretty (TCons TTuple ts) = "(" ++ intercalate ", " (map pretty ts) ++ ")"
  pretty _ = error "Unknown type for pretty"
             
instance Pretty TScheme where
  pretty (TScheme vars t) = forall ++ pretty t
      where forall = if null vars then "" else "forall " ++ unwords (map pretty vars) ++ ". "

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty (Left x) = "Error: " ++ pretty x
    pretty (Right x) = pretty x

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
-- Left "Could not unify: TBoolean with TNumber"
--
-- The following should succeed because the second assignment to x is: x := \z1 -> z1, which is still general enough to satisfy both (x 1) and (x True)
--
-- >>> test $ ELet "x" (EAbs "z" (EVar "z")) (ELet "y" (ETuple [EApp (EVar "x") (ELit (LitNumber 2)), EApp (EVar "x") (ELit (LitBoolean True))]) (EAssign "x" (EAbs "z1" (EVar "z1")) (ETuple [EVar "x", EVar "y"])))
-- Right (TCons TTuple [TCons TFunc [TBody (TVar 12),TBody (TVar 12)],TCons TTuple [TBody TNumber,TBody TBoolean]])
--
-- The following should also succeed because "x" is only ever used like this: (x True). The second assignment to x is: x := \z1 -> False, which is specific but matches the usage. Note that x's type is collapsed to: Boolean -> Boolean.
--
-- >>> test $ ELet "x" (EAbs "z" (EVar "z")) (ELet "y" (EApp (EVar "x") (ELit (LitBoolean True))) (EAssign "x" (EAbs "z1" (ELit (LitBoolean False))) (ETuple [EVar "x", EVar "y"])))
-- Right (TCons TTuple [TCons TFunc [TBody TBoolean,TBody TBoolean],TBody TBoolean])
--


-- | 'test' is a utility function for running the following tests:
--
-- >>> test $ ETuple [ELit (LitBoolean True), ELit (LitNumber 2)]
-- Right (TCons TTuple [TBody TBoolean,TBody TNumber])
--
-- >>> test $ ELet "id" (EAbs "x" (EVar "x")) (EAssign "id" (EAbs "y" (EVar "y")) (EVar "id"))
-- Right (TCons TFunc [TBody (TVar 5),TBody (TVar 5)])
--
-- >>> test $ ELet "id" (EAbs "x" (EVar "x")) (EAssign "id" (ELit (LitBoolean True)) (EVar "id"))
-- Left "Could not unify: TBoolean with 3 -> 3"
--
-- >>> test $ ELet "x" (ELit (LitBoolean True)) (EAssign "x" (ELit (LitBoolean False)) (EVar "x"))
-- Right (TBody TBoolean)
--
-- >>> test $ ELet "x" (ELit (LitBoolean True)) (EAssign "x" (ELit (LitNumber 3)) (EVar "x"))
-- Left "Could not unify: TNumber with TBoolean"
--
-- >>> test $ ELet "x" (EArray [ELit $ LitBoolean True]) (EVar "x")
-- Right (TCons TArray [TBody TBoolean])
--
-- >>> test $ ELet "x" (EArray [ELit $ LitBoolean True, ELit $ LitBoolean False]) (EVar "x")
-- Right (TCons TArray [TBody TBoolean])
--
-- >>> test $ ELet "x" (EArray []) (EAssign "x" (EArray []) (EVar "x"))
-- Right (TCons TArray [TBody (TVar 5)])
--
-- >>> test $ ELet "x" (EArray [ELit $ LitBoolean True, ELit $ LitNumber 2]) (EVar "x")
-- Left "Could not unify: TNumber with TBoolean"
--
-- >>> test $ ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
-- Right (TCons TFunc [TBody (TVar 6),TBody (TVar 6)])
--
-- >>> test $ ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EApp (EVar "id") (EVar "id")) (ELit (LitNumber 2)))
-- Right (TBody TNumber)
--
-- >>> test $ ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
-- Left "Occurs check failed: 1 in 1 -> 2"
--
-- >>> test $ EAbs "m" (ELet "y" (EVar "m") (ELet "x" (EApp (EVar "y") (ELit (LitBoolean True))) (EVar "x")))
-- Right (TCons TFunc [TCons TFunc [TBody TBoolean,TBody (TVar 3)],TBody (TVar 3)])
--
-- >>> test $ EApp (ELit (LitNumber 2)) (ELit (LitNumber 2))
-- Left "Could not unify: TNumber with TNumber -> 1"
--
-- >>> test $ ELet "x" (EAbs "y" (ELit (LitNumber 0))) (EAssign "x" (EAbs "y" (EVar "y")) (EVar "x"))
-- Right (TCons TFunc [TBody TNumber,TBody TNumber])
--
-- >>> test $ ELet "x" (EAbs "y" (EVar "y")) (EAssign "x" (EAbs "y" (ELit (LitNumber 0))) (EVar "x"))
-- Right (TCons TFunc [TBody TNumber,TBody TNumber])
--
-- >>> test $ ELet "x" (EAbs "y" (EVar "y")) (ETuple [EApp (EVar "x") (ELit (LitNumber 2)), EApp (EVar "x") (ELit (LitBoolean True))])
-- Right (TCons TTuple [TBody TNumber,TBody TBoolean])
--
-- >>> test $ ELet "x" (EAbs "y" (EVar "y")) (EApp (EVar "x") (EVar "x"))
-- Right (TCons TFunc [TBody (TVar 5),TBody (TVar 5)])
--
test :: Exp -> Either String (Type TBody)
test e = runInfer $ typeInference Map.empty e
         --in pretty e ++ " :: " ++ pretty t ++ "\n"
--     case res of
--       Left err -> putStrLn $ show e ++ "\n " ++ err ++ "\n"
--       Right t -> putStrLn $ show e ++ " :: " ++ show t ++ "\n"
    

-- Test runner

--return []

-- $( derive makeArbitrary ''TBody )
-- $( derive makeArbitrary ''Type )

--runAllTests = $(quickCheckAll)
       
