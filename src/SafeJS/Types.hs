{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs  #-}

module SafeJS.Types
       (Exp(..)
       , LitVal(..)
       , EVarName
       , TVarName
       , TBody(..)
       , TConsName(..)
       , TypeId(..)
       , Type
       , Fix(..)
       , replaceFix
       , FType(..)
       , TypeError(..)
       , InferState(..)
       , RowTVar(..)
       , liftRowTVar
       , TRowList(..)
       , TScheme(..)
       , TypeEnv
       , Substable(..)
       , flattenRow
       , unflattenRow
       , TSubst
       , VarId(..)
       , NameSource(..)
       , VarNames(freeTypeVars, mapVarNames)
       , EPropName
       ) where

import           Data.Foldable   (Foldable (..), foldr)
import           Data.Traversable   (Traversable (..))
import qualified Data.Map.Lazy   as Map
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as Set
import qualified Text.Parsec.Pos as Pos
import Prelude hiding (foldr)

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
           | EApp a (Exp a) [Exp a]
           | EAbs a [EVarName] (Exp a)
           | ELet a EVarName (Exp a) (Exp a)
           | ELit a LitVal
           | EAssign a EVarName (Exp a) (Exp a)
           | EPropAssign a (Exp a) EPropName (Exp a) (Exp a)
           | EIndexAssign a (Exp a) (Exp a) (Exp a) (Exp a)
           | EArray a [Exp a]
           | ETuple a [Exp a]
           | ERow a Bool [(EPropName, Exp a)]
           | EIfThenElse a (Exp a) (Exp a) (Exp a) -- TODO replace with ECase
           | EProp a (Exp a) EPropName
             -- TODO EIndex should not be part of the AST. should be a builtin function using
             -- pattern matching instead
           | EIndex a (Exp a) (Exp a)
             -- TODO consider better options for causing rows to become closed outside the 'new' call
           | ENew a (Exp a) [Exp a]
             deriving (Show, Eq, Ord, Functor, Foldable)

----------------------------------------------------------------------

type TVarName = Int

data TBody = TVar TVarName
           | TNumber | TBoolean | TString | TRegex | TUndefined | TNull
             deriving (Show, Eq, Ord)

newtype TypeId = TypeId TVarName
                deriving (Show, Eq, Ord)

data TConsName = TFunc | TArray | TTuple | TName TypeId
                 deriving (Show, Eq, Ord)

newtype RowTVar = RowTVar { getRowTVar :: TVarName }
                deriving (Show, Eq, Ord)

liftRowTVar :: (TVarName -> TVarName) -> RowTVar -> RowTVar
liftRowTVar f (RowTVar x) = RowTVar (f x)

-- | Row type.
data TRowList t = TRowProp EPropName t (TRowList t)
                | TRowEnd (Maybe RowTVar)
                  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data FType t = TBody TBody
             | TCons TConsName [t]
             | TRow (TRowList t)
               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype Fix f = Fix { unFix :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (Fix x) = "Fix (" ++ (show x) ++ ")"
instance Eq (f (Fix f)) => Eq (Fix f) where
  a == b = unFix a == unFix b
instance Ord (f (Fix f)) => Ord (Fix f) where
  (Fix x) `compare` (Fix y) = x `compare` y

fmapReplace :: (Functor f, Eq (f a)) => (f a -> f b -> a -> b) -> f a -> f b -> f a -> f b
fmapReplace recurse tsource tdest t =
  if t == tsource
  then tdest
  else fmap (recurse tsource tdest) t

replaceFix :: (Functor f, Eq (f (Fix f))) => f (Fix f) -> f (Fix f) -> Fix f -> Fix f
replaceFix tsource tdest (Fix t') = Fix $ fmapReplace replaceFix tsource tdest t'

type Type = Fix FType

type TSubst = Map.Map TVarName (Type)

data TypeError = TypeError { source :: Pos.SourcePos, message :: String }
               deriving (Show, Eq, Ord)

----------------------------------------------------------------------

class VarNames a where
  freeTypeVars :: a -> Set.Set TVarName
  mapVarNames :: (TVarName -> TVarName) -> a -> a

  freeTypeVars' :: (VarNames a, Foldable f) => f a -> Set.Set TVarName
  freeTypeVars' = foldr (Set.union . freeTypeVars) Set.empty

  mapVarNames' :: (VarNames a, Functor f) => (TVarName -> TVarName) -> f a -> f a
  mapVarNames' f = fmap (mapVarNames f)


instance VarNames (TVarName) where
  freeTypeVars = Set.singleton
  mapVarNames f x = f x

instance VarNames (TBody) where
  mapVarNames f (TVar x) = TVar $ f x
  mapVarNames _ t = t

  freeTypeVars (TVar n) = Set.singleton n
  freeTypeVars _ = Set.empty

instance VarNames t => VarNames (Map.Map a t) where
  freeTypeVars = freeTypeVars'
  mapVarNames = mapVarNames'
instance VarNames t => VarNames [t] where
  freeTypeVars = freeTypeVars'
  mapVarNames = mapVarNames'
instance VarNames t => VarNames (a, t) where
  freeTypeVars = freeTypeVars'
  mapVarNames = mapVarNames'
instance VarNames t => VarNames (Exp (a, t)) where
  freeTypeVars = freeTypeVars'
  mapVarNames = mapVarNames'

-- | VarNames instance for TRowList
--
-- >>> freeTypeVars (TRowProp "x" (Fix $ TBody TNumber) (TRowEnd $ Just 1))
-- fromList [1]
-- >>> freeTypeVars (TRowProp "x" (Fix $ TBody $ TVar 2) (TRowEnd Nothing))
-- fromList [2]
-- >>> freeTypeVars (TRowProp "x" (Fix $ TBody $ TVar 2) (TRowEnd $ Just 1))
-- fromList [1,2]
-- >>> freeTypeVars (TRowProp "x" (Fix $ TBody $ TVar 2) (TRowProp "y" (Fix $ TBody $ TVar 3) (TRowEnd $ Just 1)))
-- fromList [1,2,3]
instance VarNames t => VarNames (TRowList t) where
  freeTypeVars (TRowEnd (Just (RowTVar n))) = Set.singleton n
  freeTypeVars (TRowEnd _) = Set.empty
  freeTypeVars (TRowProp _ t r) = Set.union (freeTypeVars t) (freeTypeVars r)

  mapVarNames f (TRowEnd n) = TRowEnd $ fmap (liftRowTVar f) n
  mapVarNames f (TRowProp n t r) = TRowProp n (mapVarNames f t) (mapVarNames f r)

-- | VarNames instance for Type t
--
-- >>> freeTypeVars (Fix $ TBody TNumber)
-- fromList []
-- >>> freeTypeVars (Fix $ TBody $ TVar 0)
-- fromList [0]
-- >>> freeTypeVars (Fix $ TCons TFunc [Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1])
-- fromList [0,1]
-- >>> freeTypeVars (Fix $ TCons TFunc [Fix $ TBody $ TVar 1])
-- fromList [1]
-- >>> freeTypeVars $ (Fix $ (TRow (TRowEnd (Just 3))) :: Type)
-- fromList [3]
instance VarNames Type where
  freeTypeVars (Fix (TBody b)) = freeTypeVars b
  freeTypeVars (Fix (TRow trlist)) = freeTypeVars trlist
  freeTypeVars (Fix t) = freeTypeVars' t

  mapVarNames f (Fix (TBody b)) = Fix $ TBody $ mapVarNames f b
  mapVarNames f (Fix (TRow trlist)) = Fix $ TRow $ mapVarNames f trlist
  mapVarNames f (Fix t) = Fix $ mapVarNames' f t

instance VarNames (FType (Fix FType)) where
  freeTypeVars = freeTypeVars . Fix
  mapVarNames f = unFix . mapVarNames f . Fix

-- instance VarNames a => VarNames (FType a) where
--   freeTypeVars = freeTypeVars'
--   --                (TBody t) = freeTypeVars t
--   -- freeTypeVars (TCons _ ts) = freeTypeVars ts
--   -- freeTypeVars (TRow r) = freeTypeVars r

--   mapVarNames = mapVarNames'
--   -- mapVarNames f (TBody t) = TBody $ mapVarNames f t
--   -- mapVarNames f (TCons n ts) = TCons n $ mapVarNames f ts
--   -- mapVarNames f (TRow r) = TRow $ mapVarNames f r


----------------------------------------------------------------------

class Substable a where
  applySubst :: TSubst -> a -> a

  applySubst' :: (Functor f, Substable a) => TSubst -> f a -> f a
  applySubst' s = fmap $ applySubst s

-- for convenience only:
instance Substable a => Substable (Maybe a) where
  applySubst = applySubst'
instance Substable a => Substable [a] where
  applySubst = applySubst'
instance Substable a => Substable (Map.Map b a) where
  applySubst = applySubst'
instance Substable b => Substable (a, b) where
  applySubst = applySubst'

instance (Ord a, Substable a) => Substable (Set.Set a) where
  applySubst s = Set.map (applySubst s)

----------------------------------------------------------------------

-- | applySubst for Types
-- >>> applySubst (Map.fromList [(0, Fix $ TBody TNumber)]) (Fix $ TBody $ TVar 0)
-- Fix (TBody TNumber)
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TBody $ TVar 0)
-- Fix (TRow (TRowEnd Nothing))
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TRow $ TRowEnd $ Just 0)
-- Fix (TRow (TRowEnd Nothing))
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TRow $ TRowProp "bla" (Fix $ TBody TString) (TRowEnd $ Just 0))
-- Fix (TRow (TRowProp "bla" Fix (TBody TString) (TRowEnd Nothing)))
instance Substable Type where
  applySubst :: TSubst -> Type -> Type
  applySubst s ft@(Fix t) =
    case t of
     TBody (TVar n) -> substT' t n
     TRow r -> Fix $ TRow $ applySubst s r
     _ -> if ft `elem` (Map.elems s)
          then ft
          else Fix $ fmap (applySubst s) t
     where substT' defaultT n = fromMaybe (Fix defaultT) $ Map.lookup n s
    --traverse (fmap f) t
    --where f t@(TBody (TVar n)) = t --fromMaybe t $ Map.lookup n s
     --     f t = t
  -- applySubst s t@(TBody (TVar n)) = fromMaybe t $ Map.lookup n s
  -- applySubst _ t@(TBody _) = t
  -- applySubst s (TCons n ts) = TCons n (applySubst s ts)
  -- applySubst s (TRow r) = TRow $ applySubst s r

----------------------------------------------------------------------

sortRow :: TRowList t -> TRowList t
sortRow row = row -- TODO implement

flattenRow :: TRowList t -> (Map.Map EPropName t, Maybe RowTVar)
flattenRow = flattenRow' (Map.empty, Nothing)
    where flattenRow' :: (Map.Map EPropName t, Maybe RowTVar) -> TRowList t -> (Map.Map EPropName t, Maybe RowTVar)
          flattenRow' (m,r) (TRowProp n t rest) = flattenRow' (Map.insert n t m, r) rest
          flattenRow' (m,_) (TRowEnd r') = (m, r')

unflattenRow :: Map.Map EPropName t -> Maybe RowTVar -> (EPropName -> Bool) -> TRowList t
unflattenRow m r f = Map.foldrWithKey (\n t l -> if f n then TRowProp n t l else l) (TRowEnd r) m

instance Substable (TRowList Type) where
  applySubst s (TRowProp propName propType rest) = sortRow $ TRowProp propName (applySubst s propType) (applySubst s rest)
  applySubst s t@(TRowEnd (Just (RowTVar tvarName))) =
    case Map.lookup tvarName s of
      Nothing -> t
      Just (Fix (TRow tRowList)) -> tRowList
      -- UGLY HACK! REMOVE THIS, IT'S WRONG
      Just (Fix (TCons (TName (TypeId n)) _)) -> TRowEnd (Just $ RowTVar n)
      Just t' -> error $ "Cannot subst row variable into non-row: " ++ show t'
  applySubst _ (TRowEnd Nothing) = TRowEnd Nothing

----------------------------------------------------------------------

-- | Type scheme: a type expression with a "forall" over some type variables that may appear in it (universal quantification).
data TScheme = TScheme { schemeVars :: [TVarName], schemeType :: Type }
             deriving (Show, Eq)

-- | VarNames instance for TScheme
--
-- >>> freeTypeVars $ TScheme [0, 1] (Fix $ TBody $ TVar 2)
-- fromList [2]
-- >>> freeTypeVars $ TScheme [0, 1] (Fix $ TBody $ TVar 1)
-- fromList []
-- >>> freeTypeVars $ TScheme [0] (Fix $ TBody $ TVar 1)
-- fromList [1]
-- >>> freeTypeVars $ TScheme [0] (Fix $ TBody $ TVar 0)
-- fromList []
-- >>> freeTypeVars $ TScheme [] (Fix $ TBody $ TVar 1)
-- fromList [1]
-- >>> freeTypeVars $ TScheme [] (Fix $ TBody $ TNumber)
-- fromList []
-- >>> freeTypeVars $ TScheme [1] (Fix $ TBody $ TNumber)
-- fromList []
instance VarNames TScheme where
  freeTypeVars (TScheme qvars t) = freeTypeVars t `Set.difference` Set.fromList qvars
  mapVarNames f (TScheme qvars t) = TScheme (map f qvars) (mapVarNames f t)

instance Substable TScheme where
  -- | When subsituting on a TScheme, we allow replacing quantified vars!
  -- (i.e. we don't do (foldr Map.delete s qvars) $ applySubst t)
  applySubst s (TScheme qvars t) = TScheme qvars $ applySubst s t


newtype VarId = VarId Int
                deriving (Show, Eq, Ord)

-- | Type environment: maps AST variables (not type variables!) to quantified type schemes.
--
-- Note: instance of Substable
type TypeEnv = Map.Map EVarName VarId

-- Used internally to generate fresh type variable names
data NameSource = NameSource { lastName :: TVarName }
                deriving (Show, Eq)


data InferState = InferState { nameSource   :: NameSource
                             -- must be stateful because we sometimes discover that a variable is mutable.
                             , varSchemes   :: Map.Map VarId TScheme
                             , varInstances :: Map.Map TVarName (Set.Set (Type))
                             , namedTypes   :: Map.Map TypeId TScheme }
                  deriving (Show, Eq)

-- | VarNames instance for InferState
-- >>> mapVarNames (\k -> k + 1) $ InferState { nameSource = NameSource 0, varSchemes = Map.empty, varInstances = Map.fromList [(0, Set.fromList [Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1]), (1, Set.fromList [Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1])] }
-- InferState {nameSource = NameSource {lastName = 0}, varSchemes = fromList [], varInstances = fromList [(1,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2))]),(2,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2))])]}
instance VarNames InferState where
  freeTypeVars = freeTypeVars . varSchemes
  mapVarNames f is = is { varSchemes = mapVarNames f $ varSchemes is
                        , varInstances = Map.fromList $ map (\(k,v) -> (f k, Set.map (mapVarNames f) v)) $ Map.assocs $ varInstances is
                        }

instance Substable InferState where
  applySubst s is = is { varSchemes = applySubst s (varSchemes is)
                       , varInstances = Map.fromList $ map (\(k,v) -> (k, (applySubst s v) `Set.union` v)) $ Map.assocs $ varInstances is }
