{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Types2 where

import Data.Functor((<$>))
import Control.Monad.State(State, runState, get, modify)
import qualified Data.Map.Lazy as Map
import Control.Monad(forM)
import Data.Maybe(fromMaybe)

----------------------------------------------------------------------

type VarName = String

data LitVal = LitNumber Double | LitBoolean Bool | LitString String
            deriving (Show, Eq)
 
data Exp = Var VarName
         | App Exp Exp
         | Lam VarName Exp
         | Let VarName Exp Exp
         | Lit LitVal
         deriving (Show, Eq)
----------------------------------------------------------------------

type TVarName = Int

 
data TBody = TVar TVarName
            | TNumber | TBoolean | TString 
            deriving (Show, Eq)

data Type t = TBody t
            | TFunc (Type t) (Type t)
            deriving (Show, Eq, Functor)--, Foldable, Traversable)

----------------------------------------------------------------------

-- | Type scheme: a type expression with a "forall" over some type variables that may appear in it (universal quantification).
data TScheme = TQual [TVarName] (Type TBody)
             deriving (Show, Eq)

-- Used internally to generate fresh type variable names
data NameSource = NameSource { lastName :: TVarName }
                deriving (Show, Eq)

-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = State NameSource a

runInfer :: Infer a -> a
runInfer inf = fst . runState inf $ (NameSource { lastName = 0 })

fresh :: Infer TVarName
fresh = do
  modify (\ns -> ns { lastName = lastName ns + 1 })
  lastName <$> get

-- | Instantiate a type scheme by giving fresh names to all quantified type variables.
--
-- For example:
--
-- >>> runInfer . instantiate $ TQual [0] (TFunc (TBody (TVar 0)) (TBody TNumber))
-- TFunc (TBody (TVar 1)) (TBody TNumber)
--
instantiate :: TScheme -> Infer (Type TBody)
instantiate (TQual tvarNames t) = do
  allocNames <- forM tvarNames $ \tvName -> do
    freshName <- fresh
    return (tvName, freshName)

  let replaceVar (TVar n) = TVar . fromMaybe n $ lookup n allocNames
      replaceVar x = x

  return $ fmap replaceVar t

----------------------------------------------------------------------

type TSubst = Map.Map TVarName TScheme

-- addSubst :: TVarName -> TConsVal -> TSubst -> TSubst
-- addSubst = Map.insert

applySubst :: TVarName -> TSubst -> Maybe TScheme
applySubst = Map.lookup

--composeSubst :: TSubst -> TSubst -> TSubst
--composeSubst outer inner = (Map.map applySubst
