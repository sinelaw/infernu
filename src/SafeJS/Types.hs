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
       , Substable(..)
       , flattenRow
       , unflattenRow
       , TSubst
       , VarId
       , NameSource(..)
       , VarNames(..)
       , EPropName
       ) where

import           Data.Foldable   (Foldable (..), foldr)
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
                  deriving (Show, Eq, Ord, Functor, Foldable)--, Foldable, Traversable)

data Type t = TBody t
            | TCons TConsName [Type t]
            | TRow (TRowList t)
              deriving (Show, Eq, Ord, Functor, Foldable)--, Foldable, Traversable)

type TSubst = Map.Map TVarName (Type TBody)

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
-- >>> freeTypeVars (TRowProp "x" (TBody TNumber) (TRowEnd $ Just 1))
-- fromList [1]
-- >>> freeTypeVars (TRowProp "x" (TBody $ TVar 2) (TRowEnd Nothing))
-- fromList [2]
-- >>> freeTypeVars (TRowProp "x" (TBody $ TVar 2) (TRowEnd $ Just 1))
-- fromList [1,2]
-- >>> freeTypeVars (TRowProp "x" (TBody $ TVar 2) (TRowProp "y" (TBody $ TVar 3) (TRowEnd $ Just 1)))
-- fromList [1,2,3]
instance VarNames t => VarNames (TRowList t) where
  freeTypeVars (TRowEnd (Just n)) = Set.singleton n
  freeTypeVars (TRowEnd _) = Set.empty
  freeTypeVars (TRowProp _ t r) = Set.union (freeTypeVars t) (freeTypeVars r)

  mapVarNames f (TRowEnd n) = TRowEnd $ fmap f n
  mapVarNames f (TRowProp n t r) = TRowProp n (mapVarNames f t) (mapVarNames f r)

-- | VarNames instance for Type t
--
-- >>> freeTypeVars (TBody TNumber)
-- fromList []
-- >>> freeTypeVars (TBody $ TVar 0)
-- fromList [0]
-- >>> freeTypeVars (TCons TFunc [TBody $ TVar 0, TBody $ TVar 1])
-- fromList [0,1]
-- >>> freeTypeVars (TCons TFunc [TBody $ TVar 1])
-- fromList [1]
-- >>> freeTypeVars $ ((TRow (TRowEnd (Just 3))) :: Type TBody)
-- fromList [3]
instance VarNames t => VarNames (Type t) where
  freeTypeVars (TBody t) = freeTypeVars t
  freeTypeVars (TCons _ ts) = freeTypeVars ts
  freeTypeVars (TRow r) = freeTypeVars r

  mapVarNames f (TBody t) = TBody $ mapVarNames f t
  mapVarNames f (TCons n ts) = TCons n $ mapVarNames f ts
  mapVarNames f (TRow r) = TRow $ mapVarNames f r

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

instance Substable (Type TBody) where
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

instance Substable (TRowList TBody) where
  applySubst s (TRowProp propName propType rest) = sortRow $ TRowProp propName (applySubst s propType) (applySubst s rest)
  applySubst s t@(TRowEnd (Just tvarName)) = case Map.lookup tvarName s of
                                               Nothing -> t
                                               Just (TRow t') -> t'
                                               Just t' -> error $ "Cannot subst row variable into non-row: " ++ show t'
  applySubst _ (TRowEnd Nothing) = TRowEnd Nothing

----------------------------------------------------------------------

-- | Type scheme: a type expression with a "forall" over some type variables that may appear in it (universal quantification).
data TScheme = TScheme { schemeVars :: [TVarName], schemeType :: Type TBody }
             deriving (Show, Eq)

-- | VarNames instance for TScheme
--
-- >>> freeTypeVars $ TScheme [0, 1] (TBody $ TVar 2)
-- fromList [2]
-- >>> freeTypeVars $ TScheme [0, 1] (TBody $ TVar 1)
-- fromList []
-- >>> freeTypeVars $ TScheme [0] (TBody $ TVar 1)
-- fromList [1]
-- >>> freeTypeVars $ TScheme [0] (TBody $ TVar 0)
-- fromList []
-- >>> freeTypeVars $ TScheme [] (TBody $ TVar 1)
-- fromList [1]
-- >>> freeTypeVars $ TScheme [] (TBody $ TNumber)
-- fromList []
-- >>> freeTypeVars $ TScheme [1] (TBody $ TNumber)
-- fromList []
instance VarNames TScheme where
  freeTypeVars (TScheme qvars t) = freeTypeVars t `Set.difference` Set.fromList qvars
  mapVarNames f (TScheme qvars t) = TScheme (map f qvars) (mapVarNames f t)

instance Substable TScheme where
  -- | When subsituting on a TScheme, we allow replacing quantified vars!
  -- (i.e. we don't do (foldr Map.delete s qvars) $ applySubst t)
  applySubst s (TScheme qvars t) = TScheme qvars $ applySubst s t


type VarId = TVarName

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
                             , varInstances :: Map.Map TVarName (Set.Set (Type TBody)) }
                  deriving (Show, Eq)

instance VarNames InferState where
  freeTypeVars = freeTypeVars . varSchemes
  mapVarNames f is = is { varSchemes = mapVarNames f $ varSchemes is
                        , varInstances = Map.fromList $ map (\(k,v) -> (f k, Set.map (mapVarNames f) v)) $ Map.assocs $ varInstances is
                        }
    
instance Substable InferState where
  applySubst s is = is { varSchemes = applySubst s (varSchemes is) }

