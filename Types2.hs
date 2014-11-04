{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Types2 where

import Data.Functor((<$>))
import Control.Monad.State(State, get, modify)
import qualified Data.Map.Lazy as Map
import Control.Monad(forM)

----------------------------------------------------------------------

(|-) :: Maybe a -> a -> a
(Just x) |- _ = x
_ |- y = y

----------------------------------------------------------------------

type VarName = String

data LitVal = LitNumber Double | LitBoolean Bool | LitString String
 
data Exp = Var VarName
         | App Exp Exp
         | Lam VarName Exp
         | Let VarName Exp Exp
         | Lit LitVal

----------------------------------------------------------------------

type TVarName = Int

 
data TBody = TVar TVarName
            | TNumber | TBoolean | TString 

data Type t = TBody t
            | TFunc (Type t) (Type t)
            deriving (Show, Eq, Functor)--, Foldable, Traversable)

----------------------------------------------------------------------

data TScheme = TQual [TVarName] (Type TBody)

data NameSource = NameSource { lastName :: TVarName }
type Infer a = State NameSource a

fresh :: Infer TVarName
fresh = do
  modify (\ns -> ns { lastName = lastName ns + 1 })
  lastName <$> get
  

instantiate :: TScheme -> Infer (Type TBody)
instantiate (TQual tvarNames t) = do
  allocNames <- forM tvarNames $ \tvName -> do
                                    freshName <- fresh
                                    return (tvName, freshName)
  return $ fmap (\t' -> case t' of
                          (TVar n) -> TVar $ (lookup n allocNames |- n)
                          _ -> t') t


----------------------------------------------------------------------

type TSubst = Map.Map TVarName TScheme

-- addSubst :: TVarName -> TConsVal -> TSubst -> TSubst
-- addSubst = Map.insert

applySubst :: TVarName -> TSubst -> Maybe TScheme
applySubst = Map.lookup

--composeSubst :: TSubst -> TSubst -> TSubst
--composeSubst outer inner = (Map.map applySubst
