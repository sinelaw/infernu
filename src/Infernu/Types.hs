{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

module Infernu.Types
       (IsGen(..)
       , Source
       , Exp(..)
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
       , emptyInferState
       , RowTVar(..)
       , getRowTVar
       , liftRowTVar
       , FlatRowEnd(..)
       , TRowList(..)
       , ClassName(..)
       , Class(..)
       , TPred(..)
       , TQual(..)
       , qualEmpty
       , QualType
       , TScheme(..)
       , schemeEmpty
       , schemeFromQual
       , TypeScheme
       , TypeEnv
       , Substable(..)
       , flattenRow
       , unflattenRow
       , TSubst
       , nullSubst
       , composeSubst
       , singletonSubst
       , VarId(..)
       , NameSource(..)
       , addEquivalence
       , VarNames(freeTypeVars, mapVarNames)
       , EPropName
#ifdef QUICKCHECK
       , runAllTests
#endif
       ) where

import           Data.Foldable             (Foldable (..), foldr)
import qualified Data.Map.Lazy             as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set

import           Data.Traversable          (Traversable (..))
import           Prelude                   hiding (foldr)
import qualified Text.Parsec.Pos           as Pos

import           Infernu.Fix               (Fix (..), replaceFix)

#ifdef QUICKCHECK
import           Data.DeriveTH
import           Data.Functor              ((<$>))
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as Map
import           Test.QuickCheck           (choose, resize)
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
#endif

newtype IsGen = IsGen Bool
              deriving (Show, Eq, Ord)

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
           | EStringMap a [(String, Exp a)]
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

data TConsName = TFunc | TArray | TTuple | TName TypeId | TStringMap
                 deriving (Show, Eq, Ord)

newtype RowTVar = RowTVar TVarName
                deriving (Show, Eq, Ord)

getRowTVar :: RowTVar -> TVarName
getRowTVar (RowTVar x) = x

liftRowTVar :: (TVarName -> TVarName) -> RowTVar -> RowTVar
liftRowTVar f (RowTVar x) = RowTVar (f x)

-- | Row type.
data TRowList t = TRowProp EPropName (TScheme t) (TRowList t)
                | TRowEnd (Maybe RowTVar)
                | TRowRec TypeId [t]
                  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data FType t = TBody TBody
             | TCons TConsName [t]
             | TRow (TRowList t)
               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Type = Fix FType

type Source = (IsGen, Pos.SourcePos)

data TypeError = TypeError { source :: Source, message :: String }
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
-- >>> freeTypeVars (TRowProp "x" (schemeEmpty $ Fix $ TBody TNumber) (TRowEnd $ Just $ RowTVar 1))
-- fromList [1]
-- >>> freeTypeVars (TRowProp "x" (schemeEmpty $ Fix $ TBody $ TVar 2) (TRowEnd Nothing))
-- fromList [2]
-- >>> freeTypeVars (TRowProp "x" (schemeEmpty $ Fix $ TBody $ TVar 2) (TRowEnd $ Just $ RowTVar 1))
-- fromList [1,2]
-- >>> freeTypeVars (TRowProp "x" (schemeEmpty $ Fix $ TBody $ TVar 2) (TRowProp "y" (schemeEmpty $ Fix $ TBody $ TVar 3) (TRowEnd $ Just $ RowTVar 1)))
-- fromList [1,2,3]
instance VarNames t => VarNames (TRowList t) where
  freeTypeVars (TRowEnd (Just (RowTVar n))) = Set.singleton n
  freeTypeVars (TRowEnd _) = Set.empty
  freeTypeVars (TRowProp _ t r) = Set.union (freeTypeVars t) (freeTypeVars r)
  freeTypeVars (TRowRec _ ts) = foldr (Set.union . freeTypeVars) Set.empty ts

  mapVarNames f (TRowEnd n) = TRowEnd $ fmap (liftRowTVar f) n
  mapVarNames f (TRowProp n t r) = TRowProp n (mapVarNames f t) (mapVarNames f r)
  mapVarNames f (TRowRec tid ts) = TRowRec tid (mapVarNames f ts)

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
-- >>> freeTypeVars $ (Fix $ (TRow (TRowEnd (Just $ RowTVar 3))) :: Type)
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

type TSubst = Map.Map TVarName (Type)

nullSubst :: TSubst
nullSubst = Map.empty

-- | composeSubst should obey the law:
-- applySubst (composeSubst new old) t = applySubst new (applySubst old t)
-- >>> composeSubst (Map.fromList []) (Map.fromList [])
-- fromList []
-- >>> composeSubst (Map.fromList [(0,Fix (TBody (TVar 1)))]) (Map.fromList [])
-- fromList [(0,Fix (TBody (TVar 1)))]
-- >>> composeSubst (Map.fromList []) (Map.fromList [(0,Fix (TBody (TVar 1)))])
-- fromList [(0,Fix (TBody (TVar 1)))]
-- >>> composeSubst (Map.fromList [(1,Fix (TBody (TVar 2)))]) (Map.fromList [(0,Fix (TBody (TVar 1)))])
-- fromList [(0,Fix (TBody (TVar 2))),(1,Fix (TBody (TVar 2)))]
-- >>> composeSubst (Map.fromList [(0,Fix (TBody (TVar 1)))]) (Map.fromList [(1,Fix (TBody (TVar 2)))])
-- fromList [(0,Fix (TBody (TVar 2))),(1,Fix (TBody (TVar 2)))]
composeSubst :: TSubst -> TSubst -> TSubst
composeSubst new old = applySubst new old `Map.union` new

singletonSubst :: TVarName -> Type -> TSubst
singletonSubst = Map.singleton

#ifdef QUICKCHECK
prop_composeSubst :: TSubst -> TSubst -> Type -> Bool
prop_composeSubst new old t = applySubst (composeSubst new old) t == applySubst new (applySubst old t)
#endif

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
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TRow $ TRowEnd $ Just $ RowTVar 0)
-- Fix (TRow (TRowEnd Nothing))
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TRow $ TRowProp "bla" (schemeEmpty $ Fix $ TBody TString) (TRowEnd $ Just $ RowTVar 0))
-- Fix (TRow (TRowProp "bla" (TScheme {schemeVars = [], schemeType = Fix (TBody TString), schemePred = TPredTrue}) (TRowEnd Nothing)))
instance Substable Type where
  applySubst :: TSubst -> Type -> Type
  applySubst s ft@(Fix t) =
    case t of
     TBody (TVar n) -> substT' n t
     TRow r -> Fix $ TRow $ applySubst s r
     _ -> if ft `elem` (Map.elems s)
          then ft
          else Fix $ fmap (applySubst s) t
     where substT' n defaultT = fromMaybe (Fix defaultT) $ Map.lookup n s
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

data FlatRowEnd t = FlatRowEndTVar (Maybe RowTVar) | FlatRowEndRec TypeId [t]

flattenRow :: TRowList t -> (Map.Map EPropName (TScheme t), FlatRowEnd t)
flattenRow = flattenRow' (Map.empty, FlatRowEndTVar Nothing)
    where flattenRow' :: (Map.Map EPropName (TScheme t), FlatRowEnd t) -> TRowList t -> (Map.Map EPropName (TScheme t), FlatRowEnd t)
          flattenRow' (m,r) (TRowProp n t rest) = flattenRow' (Map.insert n t m, r) rest
          flattenRow' (m,_) (TRowEnd r') = (m, FlatRowEndTVar r')
          flattenRow' (m,_) (TRowRec tid ts) = (m, FlatRowEndRec tid ts)

unflattenRow :: Map.Map EPropName (TScheme t) -> FlatRowEnd t -> (EPropName -> Bool) -> TRowList t
unflattenRow m r f = Map.foldrWithKey (\n t l -> if f n then TRowProp n t l else l) rend m
  where rend = case r of
          FlatRowEndTVar r' -> TRowEnd r'
          FlatRowEndRec tid ts -> TRowRec tid ts

instance Substable (TRowList Type) where
  applySubst s (TRowProp propName propType rest) = sortRow $ TRowProp propName (applySubst s propType) (applySubst s rest)
  applySubst s t@(TRowEnd (Just (RowTVar tvarName))) =
    case Map.lookup tvarName s of
      Nothing -> t
      Just (Fix (TRow tRowList)) -> tRowList
      Just (Fix (TCons (TName tid) ts)) -> TRowRec tid ts
      Just (Fix (TBody (TVar n))) -> TRowEnd $ Just $ RowTVar n
      Just t' -> error $ "Cannot subst row variable into non-row: " ++ show t'
  applySubst _ (TRowEnd Nothing) = TRowEnd Nothing
  applySubst s (TRowRec tid ts) = TRowRec tid $ applySubst s ts

----------------------------------------------------------------------
newtype ClassName = ClassName String
                  deriving (Show, Eq, Ord)

data Class t = Class { --classSupers :: [ClassName],
                       classInstances :: [TScheme t] }
             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data TPred t = TPredIsIn { predClass :: ClassName, predType :: t }
             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data TQual t = TQual { qualPred :: [TPred t], qualType :: t }
             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

qualEmpty :: t -> TQual t
qualEmpty = TQual []

type QualType = TQual Type

data TScheme t = TScheme { schemeVars :: [TVarName]
                         , schemeType :: TQual t }
               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

schemeEmpty :: t -> TScheme t
schemeEmpty t = TScheme [] $ qualEmpty t

schemeFromQual :: TQual t -> TScheme t
schemeFromQual q = TScheme [] q

type TypeScheme = TScheme Type

instance VarNames t => VarNames (TQual t) where
    freeTypeVars (TQual p t) = freeTypeVars p `Set.union` freeTypeVars t
    mapVarNames f (TQual p t) = TQual (mapVarNames f p) (mapVarNames f t)

instance Substable t => Substable (TQual t) where
    applySubst s (TQual p t) = TQual (applySubst s p) (applySubst s t)

instance VarNames t => VarNames (TPred t) where
    freeTypeVars (TPredIsIn _ t) = freeTypeVars t
    mapVarNames f (TPredIsIn n t) = TPredIsIn n $ mapVarNames f t

instance Substable t => Substable (TPred t) where
    applySubst s (TPredIsIn n t) = TPredIsIn n $ applySubst s t

-- | VarNames instance for TScheme
-- >>> let sc v t = TScheme v t TPredTrue
-- >>> freeTypeVars $ sc [0, 1] (Fix $ TBody $ TVar 2)
-- fromList [2]
-- >>> freeTypeVars $ sc [0, 1] (Fix $ TBody $ TVar 1)
-- fromList []
-- >>> freeTypeVars $ sc [0] (Fix $ TBody $ TVar 1)
-- fromList [1]
-- >>> freeTypeVars $ sc [0] (Fix $ TBody $ TVar 0)
-- fromList []
-- >>> freeTypeVars $ schemeEmpty (Fix $ TBody $ TVar 1)
-- fromList [1]
-- >>> freeTypeVars $ schemeEmpty (Fix $ TBody $ TNumber)
-- fromList []
-- >>> freeTypeVars $ sc [1] (Fix $ TBody $ TNumber)
-- fromList []
instance VarNames t => VarNames (TScheme t) where
  freeTypeVars (TScheme qvars t) = (freeTypeVars t) `Set.difference` Set.fromList qvars
  mapVarNames f (TScheme qvars t) = TScheme (map f qvars) (mapVarNames f t)

instance Substable t => Substable (TScheme t) where
    applySubst = schemeForceApplySubst

-- Substitution on TScheme that doesn't touch quantified variables
schemeQApplySubst s (TScheme qvars t) = TScheme qvars $ applySubst (foldr Map.delete s qvars) t

-- Substitution on TScheme that *does* replace even quantified variables
schemeForceApplySubst s (TScheme qvars t) = TScheme qvars (applySubst s t)


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
                             , mainSubst    :: TSubst
                             -- must be stateful because we sometimes discover that a variable is mutable.
                             , varSchemes   :: Map.Map VarId TypeScheme
                             , varInstances :: Map.Map TVarName (Set.Set QualType)
                             , namedTypes   :: Map.Map TypeId (Type, TypeScheme)
                             , classes      :: Map.Map ClassName (Class Type)
                             , pendingUni   :: Set.Set (Source, Type, (ClassName, Set.Set TypeScheme))
                             }
                   deriving (Show, Eq)

emptyInferState :: InferState
emptyInferState = InferState { nameSource = NameSource 2
                             , mainSubst = nullSubst
                             , varSchemes = Map.empty
                             , varInstances = Map.empty
                             , namedTypes = Map.empty
                             , pendingUni = Set.empty
                             , classes = Map.fromList
                                         [ (ClassName "Indexable", Class { classInstances =
                                                                                   [ TScheme { schemeVars = [0]
                                                                                             , schemeType = qualEmpty
                                                                                                            $ Fix $ TCons TTuple
                                                                                                            [ Fix $ TCons TArray [Fix $ TBody $ TVar 0]
                                                                                                            , Fix $ TBody TNumber
                                                                                                            , Fix $ TBody $ TVar 0 ]
                                                                                             }
                                                                                   , TScheme { schemeVars = [1]
                                                                                             , schemeType = qualEmpty
                                                                                                            $ Fix $ TCons TTuple
                                                                                                            [ Fix $ TCons TStringMap [Fix $ TBody $ TVar 1]
                                                                                                            , Fix $ TBody TString
                                                                                                            , Fix $ TBody $ TVar 1 ]
                                                                                             }
                                                                                   , TScheme { schemeVars = []
                                                                                             , schemeType = qualEmpty
                                                                                                            $ Fix $ TCons TTuple
                                                                                                            [ Fix $ TBody TString
                                                                                                            , Fix $ TBody TNumber
                                                                                                            , Fix $ TBody TString ]
                                                                                             }
                                                                                   ] })
                                         , (ClassName "Plus", Class { classInstances =
                                                                              [ TScheme { schemeVars = []
                                                                                        , schemeType = qualEmpty $ Fix $ TBody $ TNumber
                                                                                        }
                                                                              , TScheme { schemeVars = []
                                                                                        , schemeType = qualEmpty $ Fix $ TBody $ TString
                                                                                        }
                                                                              ] })
                                         ]
                             }

-- | VarNames instance for InferState
-- >>> mapVarNames (\k -> k + 1) $ emptyInferState { varInstances = Map.fromList [(0, Set.fromList [Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1]), (1, Set.fromList [Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1])] }
-- InferState {nameSource = NameSource {lastName = 0}, mainSubst = fromList [], varSchemes = fromList [], varInstances = fromList [(1,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2))]),(2,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2))])], namedTypes = fromList []}
instance VarNames InferState where
  freeTypeVars = freeTypeVars . varSchemes
  mapVarNames f is = is { varSchemes = mapVarNames f $ varSchemes is
                        , varInstances = Map.fromList $ map (\(k,v) -> (f k, Set.map (mapVarNames f) v)) $ Map.assocs $ varInstances is
                        }

instance Substable InferState where
  applySubst s is = is { varSchemes = applySubst s (varSchemes is)
                       , mainSubst = s `composeSubst` (mainSubst is)
                       , varInstances = Map.fromList $ concatMap (\(k,v) -> entries k v) $ Map.assocs $ varInstances is }
      where updatedSet v = (applySubst s v) `Set.union` v
            entries k v = (k, v') : (map (, v') $ substedKeyEntries k)
                          where v' = updatedSet v
            substedKeyEntries k = case applySubst s (Fix $ TBody $ TVar k) of
                                      Fix (TBody (TVar m)) -> if m == k then [] else [m]
                                      _ -> []
            
-- | Adds a pair of equivalent items to an equivalence map.
--
-- >>> let m1 = addEquivalence 1 2 Map.empty
-- >>> pretty m1
-- "Map (b => Set {b, c}, c => Set {b, c})"
-- >>> pretty $ addEquivalence 1 3 m1
-- "Map (b => Set {b, c, d}, c => Set {b, c, d}, d => Set {b, c, d})"
-- >>> pretty $ addEquivalence 3 1 m1
-- "Map (b => Set {b, c, d}, c => Set {b, c, d}, d => Set {b, c, d})"
-- >>> pretty $ addEquivalence 4 5 m1
-- "Map (b => Set {b, c}, c => Set {b, c}, e => Set {e, f}, f => Set {e, f})"
-- >>> pretty $ addEquivalence 1 4 $ addEquivalence 4 5 m1
-- "Map (b => Set {b, c, e, f}, c => Set {b, c, e, f}, e => Set {b, c, e, f}, f => Set {b, c, e, f})"
addEquivalence :: TVarName -> TVarName -> Map.Map TVarName (Set.Set QualType) -> Map.Map TVarName (Set.Set QualType)
addEquivalence x y m = foldr (\k m' -> Map.insert k updatedSet m') m setTVars
    where updatedSet :: Set.Set QualType
          updatedSet = Set.insert (qualEmpty $ Fix $ TBody $ TVar x) . Set.insert (qualEmpty $ Fix $ TBody $ TVar y) $ Set.union (getSet x) (getSet y)
          getSet item = fromMaybe Set.empty $ Map.lookup item m
          setTVars :: [TVarName]
          setTVars = mapVarNames' $ Set.toList updatedSet
          mapVarNames' :: [QualType] -> [TVarName]
          mapVarNames' [] = []
          mapVarNames' (TQual _ (Fix (TBody (TVar n))) : ts) = n : mapVarNames' ts
          mapVarNames' (_:ts) = mapVarNames' ts



#ifdef QUICKCHECK
-- Test runner
return []

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = Map.fromList <$> resize 2 arbitrary
    shrink m = map (flip Map.delete m) (Map.keys m)

$( derive makeArbitrary ''TypeId )
$( derive makeArbitrary ''RowTVar )
$( derive makeArbitrary ''TRowList )
$( derive makeArbitrary ''TConsName )
$( derive makeArbitrary ''TBody )
$( derive makeArbitrary ''FType )

instance Arbitrary (Fix FType) where
    arbitrary = Fix <$> arbitrary


{-# WARNING runAllTests "QuickCheck runner, do not use!" #-}
runAllTests :: IO Bool
runAllTests = $(quickCheckAll)

#endif
