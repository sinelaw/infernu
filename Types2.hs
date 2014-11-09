{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-} -- for quickcheck all
{-# LANGUAGE TupleSections     #-}

module Types2 where

import           Control.Monad       (forM)
import           Control.Monad.State (State, evalState, get, modify)
import           Data.Functor        ((<$>))
import           Data.List           ((\\), intercalate)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromMaybe)

-- import           Test.QuickCheck(choose)
--import           Test.QuickCheck.All    
-- import           Test.QuickCheck.Arbitrary(Arbitrary(..))
-- import           Data.DeriveTH
--import Debug.Trace(traceShowId)
    
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
         deriving (Show, Eq, Ord)

----------------------------------------------------------------------

type TVarName = Int


data TBody = TVar TVarName
            | TNumber | TBoolean | TString
            deriving (Show, Eq, Ord)

data Type t = TBody t
            | TFunc (Type t) (Type t)
            deriving (Show, Eq, Ord, Functor)--, Foldable, Traversable)


type TSubst = Map.Map TVarName (Type TBody)


----------------------------------------------------------------------

class Types a where
  freeTypeVars :: a -> [TVarName]
  applySubst :: TSubst -> a -> a

-- for convenience only:
instance Types a => Types [a] where
  freeTypeVars = foldr ((++) . freeTypeVars) []
  applySubst s = map (applySubst s)

instance Types a => Types (Map.Map b a) where
  freeTypeVars m = freeTypeVars . Map.elems $ m
  applySubst s = Map.map (applySubst s)
  
----------------------------------------------------------------------

instance Types (Type TBody) where
  freeTypeVars (TBody (TVar n)) = [n]
  freeTypeVars (TBody _) = []
  freeTypeVars (TFunc t1 t2) = freeTypeVars t1 ++ freeTypeVars t2

  applySubst s t@(TBody (TVar n)) = fromMaybe t $ Map.lookup n s
  applySubst _ t@(TBody _) = t
  applySubst s (TFunc t1 t2) = TFunc (applySubst s t1) (applySubst s t2)
  
                                     
----------------------------------------------------------------------

-- | Type scheme: a type expression with a "forall" over some type variables that may appear in it (universal quantification).
data TScheme = TScheme [TVarName] (Type TBody)
             deriving (Show, Eq)

instance Types TScheme where
  freeTypeVars (TScheme qvars t) = freeTypeVars t \\ qvars
  applySubst s (TScheme qvars t) = TScheme qvars $ applySubst (foldr Map.delete s qvars) t

alphaEquivalent :: TScheme -> TScheme -> Bool                                   
alphaEquivalent ts1@(TScheme tvn1 _) (TScheme tvn2 t2) = ts1 == TScheme tvn1 ts2'
    where TScheme _ ts2' = applySubst substVarNames (TScheme [] t2)
          substVarNames = Map.fromList . map (\(old,new) -> (old, TBody $ TVar new)) $ zip tvn2 tvn1
    
----------------------------------------------------------------------

-- | Type environment: maps AST variables (not type variables!) to quantified type schemes.
--
-- Note: instance of Types 
type TypeEnv = Map.Map EVarName TScheme

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

-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = State NameSource a

runInferWith :: NameSource -> Infer a -> a
runInferWith ns inf = evalState inf ns

runInfer :: Infer a -> a
runInfer = runInferWith NameSource { lastName = 0 }

fresh :: Infer TVarName
fresh = do
  modify (\ns -> ns { lastName = lastName ns + 1 })
  lastName <$> get

throwError :: String -> Infer a
throwError = error

-- | Instantiate a type scheme by giving fresh names to all quantified type variables.
--
-- For example:
--
-- >>> runInferWith (NameSource 2) . instantiate $ TScheme [0] (TFunc (TBody (TVar 0)) (TBody (TVar 1)))
-- TFunc (TBody (TVar 3)) (TBody (TVar 1))
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
-- >>> let t = TScheme [0] (TFunc (TBody (TVar 0)) (TBody (TVar 1)))
-- >>> let tenv = Map.insert "x" t Map.empty
-- >>> tenv
-- fromList [("x",TScheme [0] (TFunc (TBody (TVar 0)) (TBody (TVar 1))))]
-- >>> generalize tenv (TFunc (TBody (TVar 1)) (TBody (TVar 2)))
-- TScheme [2] (TFunc (TBody (TVar 1)) (TBody (TVar 2)))
--
-- In this example the steps were:
--
-- 1. Environment: { x :: forall 0. 0 -> 1 }
--
-- 2. generalize (1 -> 2)
--
-- 3. result: forall 2. 1 -> 2
--
-- >>> generalize Map.empty (TFunc (TBody (TVar 0)) (TBody (TVar 0)))
-- TScheme [0] (TFunc (TBody (TVar 0)) (TBody (TVar 0)))
--
generalize :: TypeEnv -> Type TBody -> TScheme
generalize tenv t = TScheme (freeTypeVars t \\ freeTypeVars tenv) t

----------------------------------------------------------------------

unify :: Type TBody -> Type TBody -> Infer TSubst
unify (TBody (TVar n)) t = varBind n t
unify t (TBody (TVar n)) = varBind n t
unify (TBody x) (TBody y) = if x == y
                            then return nullSubst
                            else throwError $ "Could not unify: " ++ show x ++ " with " ++ show y
unify t1@(TBody _) t2@(TFunc _ _) = throwError $ "Could not unify: " ++ show t1 ++ " with " ++ show t2
unify t1@(TFunc _ _) t2@(TBody _) = unify t2 t1
unify (TFunc t1 t2) (TFunc u1 u2) = do s1 <- unify t1 u1
                                       s2 <- unify (applySubst s1 t2) (applySubst s1 u2)
                                       return (s2 `composeSubst` s1)
  
varBind :: TVarName -> Type TBody -> Infer TSubst
varBind n t | t == TBody (TVar n) = return nullSubst
            | n `elem` freeTypeVars t = throwError $ "Occurs check failed: " ++ show n ++ " in " ++ show t
            | otherwise = return $ singletonSubst n t

              
----------------------------------------------------------------------

inferType :: TypeEnv -> Exp -> Infer (TSubst, Type TBody)
inferType _ (ELit lit) = return . (nullSubst,) $ TBody $ case lit of
  LitNumber _ -> TNumber
  LitBoolean _ -> TBoolean
  LitString _ -> TString
inferType env (EVar n) = case Map.lookup n env of
  Nothing -> throwError $ "Unbound variable: " ++ n
  Just ts -> (nullSubst,) <$> instantiate ts
inferType env (EAbs argName e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
         env' = Map.insert argName (TScheme [] tvar) env
     (s1, t1) <- inferType env' e2
     return (s1, TFunc (applySubst s1 tvar) t1)
inferType env (EApp e1 e2) =
  do tvarName <- fresh
     let tvar = TBody (TVar tvarName)
     (s1, t1) <- inferType env e1
     (s2, t2) <- inferType (applySubst s1 env) e2
     s3 <- unify (applySubst s2 t1) (TFunc t2 tvar)
     return (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tvar)
inferType env (ELet n e1 e2) =
  do (s1, t1) <- inferType env e1
     let t' = generalize (applySubst s1 env) t1
         env' = Map.insert n t' env
     (s2, t2) <- inferType env' e2
     return (s2 `composeSubst` s1, t2)
inferType env (EAssign n e1 e2) =
  do (s1, t1) <- inferType env e1
     let ts' = generalize (applySubst s1 env) t1
     case Map.lookup n env of
       Nothing -> throwError $ "Unboud variable: " ++ n
       Just ts -> if ts `alphaEquivalent` ts'
                  then inferType env e2
                  else throwError $ "Polymorphic types do not match: Actual = " ++ pretty ts ++ ", Expected = " ++ pretty ts'

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
                             
instance Pretty TVarName where
  pretty = show

instance Pretty TBody where
  pretty (TVar n) = pretty n
  pretty x = show x

instance Pretty t => Pretty (Type t) where
  pretty (TBody t) = pretty t
  pretty (TFunc t1 t2) = pretty t1 ++ " -> " ++ pretty t2

instance Pretty TScheme where
  pretty (TScheme vars t) = forall ++ pretty t
      where forall = if null vars then "" else "forall " ++ intercalate " " (map pretty vars) ++ ". "


----------------------------------------------------------------------

te0 = ELet "id" (EAbs "x" (EVar "x")) (EAssign "id" (EAbs "x" (EVar "x")) (EVar "id"))
te0bad = ELet "id" (EAbs "x" (EVar "x")) (EAssign "id" (ELit (LitBoolean True)) (EVar "id"))
litOk = ELet "x" (ELit (LitBoolean True)) (EAssign "x" (ELit (LitBoolean False)) (EVar "x"))
litBad = ELet "x" (ELit (LitBoolean True)) (EAssign "x" (ELit (LitNumber 3)) (EVar "x"))
te2 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
te3 = ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EApp (EVar "id") (EVar "id")) (ELit (LitNumber 2)))
te4 = ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
te5 = EAbs "m" (ELet "y" (EVar "m") (ELet "x" (EApp (EVar "y") (ELit (LitBoolean True))) (EVar "x")))
te6 = EApp (ELit (LitNumber 2)) (ELit (LitNumber 2))

test :: Exp -> IO ()
test e =
  do let t = runInfer $ typeInference Map.empty e
     putStrLn $ pretty e ++ " :: " ++ pretty t ++ "\n"
--     case res of
--       Left err -> putStrLn $ show e ++ "\n " ++ err ++ "\n"
--       Right t -> putStrLn $ show e ++ " :: " ++ show t ++ "\n"
    

-- Test runner

--return []

-- $( derive makeArbitrary ''TBody )
-- $( derive makeArbitrary ''Type )

--runAllTests = $(quickCheckAll)
       
