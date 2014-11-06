{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-} -- for quickcheck all

module Types2 where

import           Control.Monad       (forM)
import           Control.Monad.State (State, evalState, get, modify)
import           Data.Functor        ((<$>))
import           Data.List           ((\\))
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (fromMaybe)

-- import           Test.QuickCheck(choose)
--import           Test.QuickCheck.All    
-- import           Test.QuickCheck.Arbitrary(Arbitrary(..))
-- import           Data.DeriveTH

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
         | ELam EVarName Exp
         | ELet EVarName Exp Exp
         | ELit LitVal
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
generalize :: TypeEnv -> Type TBody -> TScheme
generalize tenv t = TScheme (freeTypeVars t \\ freeTypeVars tenv) t

----------------------------------------------------------------------
--composeSubst new old = (Map.Map (apply old) new)

-- addSubst :: TVarName -> TConsVal -> TSubst -> TSubst
-- addSubst = Map.insert

-- applySubst :: EVarName -> TypeEnv -> Maybe TScheme
-- applySubst = Map.lookup

--composeSubst :: TSubst -> TSubst -> TSubst
--composeSubst outer inner = (Map.map applySubst

----------------------------------------------------------------------
-- Test runner

--return []

-- $( derive makeArbitrary ''TBody )
-- $( derive makeArbitrary ''Type )

--runAllTests = $(quickCheckAll)
       
