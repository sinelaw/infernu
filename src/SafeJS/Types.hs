{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module SafeJS.Types
       (Exp(..)
       , LitVal(..)
       , EVarName
       , TVarName
       , TBody(..)
       , TConsName(..)
       , Type(..)
       , TypeError(..)
       , InferState(..)
       , TRowList(..)
       , TScheme(..)
       , TypeEnv
       , Types(..)
       , flattenRow
       , unflattenRow
       , TSubst
       , VarId
       , NameSource(..)
       , VarNames(..)
       , EPropName
       ) where

import           Data.Foldable   (Foldable (..))
import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as Set
import qualified Text.Parsec.Pos as Pos

type EVarName = String
type EPropName = String

data LitVal = LitNumber Double
            | LitBoolean Bool
            | LitString String
            | LitRegex String Bool Bool
            | LitUndefined
            | LitNull
            deriving (Show, Eq, Ord)

data Exp a = EVar a EVarName
           | EApp a (Exp a) (Exp a)
           | EAbs a EVarName (Exp a)
           | ELet a EVarName (Exp a) (Exp a)
           | ELit a LitVal
           | EAssign a EVarName (Exp a) (Exp a)
           | EPropAssign a (Exp a) EPropName (Exp a) (Exp a)
           | EArray a [Exp a]
           | ETuple a [Exp a]
           | ERow a [(EPropName, Exp a)]
           | EIfThenElse a (Exp a) (Exp a) (Exp a) -- TODO replace with ECase
           | EProp a (Exp a) EPropName
             -- TODO EIndex should not be part of the AST. should be a builtin function using
             -- pattern matching instead
           | EIndex a (Exp a) (Exp a) 
             deriving (Show, Eq, Ord, Functor, Foldable)

----------------------------------------------------------------------

type TVarName = Int

data TBody = TVar TVarName
           | TNumber | TBoolean | TString | TRegex | TUndefined | TNull
             deriving (Show, Eq, Ord)

data TConsName = TFunc | TArray | TTuple
                 deriving (Show, Eq, Ord)

-- | Row type.
data TRowList t = TRowProp EPropName (Type t) (TRowList t)
                | TRowEnd (Maybe TVarName)
                  deriving (Show, Eq, Ord, Functor)--, Foldable, Traversable)

data Type t = TBody t
            | TCons TConsName [Type t]
            | TRow (TRowList t)
              deriving (Show, Eq, Ord, Functor)--, Foldable, Traversable)

type TSubst = Map.Map TVarName (Type TBody)

data TypeError = TypeError { source :: Pos.SourcePos, message :: String }
               deriving (Show, Eq, Ord)

----------------------------------------------------------------------

class VarNames a where
    mapVarNames :: (TVarName -> TVarName) -> a -> a

instance VarNames (Type TBody) where
    mapVarNames f (TBody (TVar x)) = TBody . TVar $ f x
    mapVarNames _ t@(TBody _) = t
    mapVarNames f (TCons c ts) = TCons c $ map (mapVarNames f) ts
    mapVarNames f (TRow l) = TRow $ mapVarNames f l

instance VarNames (TRowList TBody) where
    mapVarNames f (TRowProp n t l) = TRowProp n (mapVarNames f t) (mapVarNames f l)
    mapVarNames f (TRowEnd (Just x)) = TRowEnd (Just $ f x)
    mapVarNames _ (TRowEnd Nothing) = TRowEnd Nothing

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
  freeTypeVars (TRow r) = freeTypeVars r

  applySubst s t@(TBody (TVar n)) = fromMaybe t $ Map.lookup n s
  applySubst _ t@(TBody _) = t
  applySubst s (TCons n ts) = TCons n (applySubst s ts)
  applySubst s (TRow r) = TRow $ applySubst s r


----------------------------------------------------------------------

sortRow :: TRowList TBody -> TRowList TBody
sortRow row = row -- TODO implement

flattenRow :: TRowList t -> (Map.Map EPropName (Type t), Maybe TVarName)
flattenRow = flattenRow' (Map.empty, Nothing)
    where flattenRow' :: (Map.Map EPropName (Type t), Maybe TVarName) -> TRowList t -> (Map.Map EPropName (Type t), Maybe TVarName)
          flattenRow' (m,r) (TRowProp n t rest) = flattenRow' (Map.insert n t m, r) rest
          flattenRow' (m,_) (TRowEnd r') = (m, r')

unflattenRow :: Map.Map EPropName (Type t) -> Maybe TVarName -> (EPropName -> Bool) -> TRowList t
unflattenRow m r f = Map.foldrWithKey (\n t l -> if f n then TRowProp n t l else l) (TRowEnd r) m

-- | Types instance for TRowList
--
-- >>> freeTypeVars (TRowProp "x" (TBody TNumber) (TRowEnd $ Just 1))
-- fromList [1]
-- >>> freeTypeVars (TRowProp "x" (TBody $ TVar 2) (TRowEnd Nothing))
-- fromList [2]
-- >>> freeTypeVars (TRowProp "x" (TBody $ TVar 2) (TRowEnd $ Just 1))
-- fromList [1,2]
-- >>> freeTypeVars (TRowProp "x" (TBody $ TVar 2) (TRowProp "y" (TBody $ TVar 3) (TRowEnd $ Just 1)))
-- fromList [1,2,3]
instance Types (TRowList TBody) where
  freeTypeVars (TRowProp _ propType rest) = freeTypeVars propType `Set.union` freeTypeVars rest
  freeTypeVars (TRowEnd tvarName) = maybe Set.empty Set.singleton tvarName

  applySubst s (TRowProp propName propType rest) = sortRow $ TRowProp propName (applySubst s propType) (applySubst s rest)
  applySubst s t@(TRowEnd (Just tvarName)) = case Map.lookup tvarName s of
                                               Nothing -> t
                                               Just (TRow t') -> t'
                                               Just t' -> error $ "Cannot subst row variable into non-row: " ++ show t'
  applySubst _ (TRowEnd Nothing) = TRowEnd Nothing

----------------------------------------------------------------------

-- | Type scheme: a type expression with a "forall" over some type variables that may appear in it (universal quantification).
data TScheme = TScheme [TVarName] (Type TBody)
             deriving (Show, Eq)

instance Types TScheme where
  freeTypeVars (TScheme qvars t) = freeTypeVars t `Set.difference` Set.fromList qvars
  -- | When subsituting on a TScheme, we allow replacing quantified vars!
  -- (i.e. we don't do (foldr Map.delete s qvars) $ applySubst t)
  applySubst s (TScheme qvars t) = TScheme qvars $ applySubst s t


type VarId = TVarName

-- | Type environment: maps AST variables (not type variables!) to quantified type schemes.
--
-- Note: instance of Types
type TypeEnv = Map.Map EVarName VarId

-- Used internally to generate fresh type variable names
data NameSource = NameSource { lastName :: TVarName }
                deriving (Show, Eq)


data InferState = InferState { nameSource   :: NameSource
                             -- must be stateful because we sometimes discover that a variable is mutable.
                             , varSchemes   :: Map.Map VarId TScheme
                             , varInstances :: Map.Map TVarName (Set.Set (Type TBody)) }
                  deriving (Show, Eq)

instance Types InferState where
    freeTypeVars = freeTypeVars . varSchemes
    applySubst s is = is { varSchemes = applySubst s (varSchemes is) }

