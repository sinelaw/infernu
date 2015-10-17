{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TupleSections        #-}

module Infernu.Types
       ( Kind(..), kindArgsNum, karrow, HasKind(..)
       , TVarName(..)
       , unTVarName
       , setTVarName
       , TBody(..)
       , TConsName(..)
       , TypeId(..)
       , Type
       , Fix(..)
       , replaceFix
       , FType(..)
       , InferState(..)
       , RowTVar(..)
       , getRowTVar
       , liftRowTVar
       , FlatRowEnd(..)
       , TProp(..)
       , tpropName
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
       , VarNames(freeTypeVars, mapVarNames)
#ifdef QUICKCHECK
       , runAllTests
#endif
       ) where


import qualified Data.Map.Strict             as Map
import Data.Map.Strict             (Map)
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set
import Data.Hashable (Hashable(..))

import GHC.Generics (Generic)

import           Infernu.Source            (Source)
import           Infernu.Expr              (Exp, EVarName, EPropName)
import           Infernu.Fix               (Fix (..), replaceFix)
import           Infernu.Prelude
import Prelude ()

#ifdef QUICKCHECK
import           Data.DeriveTH
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Test.QuickCheck           (choose, resize)
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
#endif

----------------------------------------------------------------------

data Kind = KStar | KArrow Kind Kind | KRow
          deriving (Show, Eq, Ord, Generic)

kindArgsNum :: Kind -> Int
kindArgsNum KStar = 0
kindArgsNum KRow = 0
kindArgsNum (KArrow _ k) = 1 + kindArgsNum k

kApply :: Kind -> [Kind] -> Maybe Kind
kApply k [] = Just k
kApply (KArrow karg kres) (karg':kargs)
    | karg == karg' = kApply kres kargs
    | otherwise = Nothing
kApply _ _ = Nothing

karrow :: Kind -> [Kind] -> Kind
karrow k [] = k
karrow k (karg:ks) = KArrow karg $ karrow k ks

instance Hashable Kind

data TVarName = Flex !Int Kind
              | Skolem !Int Kind
              deriving (Show, Eq, Ord, Generic)

instance Hashable TVarName

unTVarName :: TVarName -> Int
unTVarName (Flex x _) = x
unTVarName (Skolem x _) = x

setTVarName :: TVarName -> Int -> TVarName
setTVarName (Flex _ k) y = Flex y k
setTVarName (Skolem _ k) y = Skolem y k

data TBody = TVar !TVarName
           | TNumber | TBoolean | TString | TRegex | TUndefined | TNull | TEmptyThis | TDate
             deriving (Show, Eq, Ord)

newtype TypeId = TypeId Int
                deriving (Show, Eq, Ord, Generic)
instance Hashable TypeId where

data TConsName = TArray
               | TTuple
               | TName !TypeId Kind
               | TStringMap
               | TRef
                 deriving (Show, Eq, Ord)

newtype RowTVar = RowTVar TVarName
                deriving (Show, Eq, Ord)

getRowTVar :: RowTVar -> TVarName
getRowTVar (RowTVar x) = x

liftRowTVar :: (TVarName -> TVarName) -> RowTVar -> RowTVar
liftRowTVar f (RowTVar x) = RowTVar (f x)

-- | Row type.
data TProp = TPropSetName !EPropName
           | TPropGetName !EPropName
           deriving (Show, Eq, Ord, Generic)

instance Hashable TProp where

tpropName :: TProp -> EPropName
tpropName (TPropSetName x) = x
tpropName (TPropGetName x) = x

data TRowList t = TRowProp !TProp !(TScheme t) !(TRowList t)
                | TRowEnd !(Maybe RowTVar)
                | TRowRec !TypeId ![t]
                  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data FType t = TBody !TBody
             | TCons !TConsName ![t]
               -- | TFunc (functions) are Profunctor-types. Arguments could have been a single 't'
               -- and always wrapped in a Tuple - but are expanded to a list here for convenience
             | TFunc ![t] !t
               -- | Row types have an optional label, so that structural (non-nominal) types can
               -- have a name. The label has no effect on type checking.
             | TRow !(Maybe String) !(TRowList t)
               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Type = Fix FType

----------------------------------------------------------------------

class HasKind t where
    kind :: t -> Kind

instance HasKind TVarName where
    kind (Flex _ k) = k
    kind (Skolem _ k) = k

instance HasKind TBody where
    kind (TVar v) = kind v
    kind _ = KStar

instance HasKind RowTVar where
    kind _ = KRow

instance HasKind (TRowList t) where
    kind _ = KRow

instance HasKind TConsName where
    kind TArray = KArrow KStar KStar
    kind TStringMap = KArrow KStar KStar
    kind TRef = KArrow KStar KStar
    kind TTuple = KArrow KStar (KArrow KStar KStar)
    kind (TName _ k) = k

instance HasKind Type where
    kind (Fix (TBody b)) = kind b
    kind (Fix (TCons c ts)) = case kApply (kind c) (map kind ts) of
        Nothing -> error $ "Can't apply kind: " ++ show (kind c) ++ " on " ++ show ts
        Just k -> k
    kind (Fix (TFunc _ _)) = KStar
    kind (Fix (TRow _ _)) = KRow

class VarNames a where
  freeTypeVars :: a -> Set.Set TVarName
  mapVarNames :: (TVarName -> TVarName) -> a -> a

  freeTypeVars' :: (VarNames a, Foldable f) => f a -> Set.Set TVarName
  freeTypeVars' = foldr (Set.union . freeTypeVars) Set.empty

  mapVarNames' :: (VarNames a, Functor f) => (TVarName -> TVarName) -> f a -> f a
  mapVarNames' f = fmap (mapVarNames f)


instance VarNames (TVarName) where
  freeTypeVars = Set.singleton
  mapVarNames f = f

instance VarNames (TBody) where
  mapVarNames f (TVar x) = TVar (f x)
  mapVarNames _ t = t

  freeTypeVars (TVar n) = Set.singleton n
  freeTypeVars _ = Set.empty

instance VarNames t => VarNames (Map a t) where
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
-- >>> freeTypeVars (Fix $ TFunc [Fix $ TBody $ TVar 0] (Fix $ TBody $ TVar 1))
-- fromList [0,1]
-- >>> freeTypeVars (Fix $ TFunc [] (Fix $ TBody $ TVar 1))
-- fromList [1]
-- >>> freeTypeVars $ (Fix $ (TRow (TRowEnd (Just $ RowTVar 3))) :: Type)
-- fromList [3]
instance VarNames Type where
  freeTypeVars (Fix (TBody b)) = freeTypeVars b
  freeTypeVars (Fix (TRow _ trlist)) = freeTypeVars trlist
  freeTypeVars (Fix t) = freeTypeVars' t

  mapVarNames f (Fix (TBody b)) = Fix $ TBody $ mapVarNames f b
  mapVarNames f (Fix (TRow l trlist)) = Fix $ TRow l (mapVarNames f trlist)
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

type TSubst = Map TVarName Type

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
  {-# INLINE applySubst' #-}
  applySubst' = fmap . applySubst

-- for convenience only:
instance Substable a => Substable (Maybe a) where
  applySubst = {-# SCC "applySubst_Maybe" #-} applySubst'
instance Substable a => Substable [a] where
  applySubst = {-# SCC "applySubst_[]" #-} applySubst'
instance Substable a => Substable (Map b a) where
  applySubst s m = {-# SCC "applySubst_Map" #-} Map.map (applySubst s) m
instance Substable b => Substable (a, b) where
  applySubst = {-# SCC "applySubst(,)" #-} applySubst'

instance (Ord a, Substable a) => Substable (Set.Set a) where
  applySubst s = {-# SCC "applySubst_Set" #-} Set.map (applySubst s)

----------------------------------------------------------------------

-- | applySubst for Types
-- >>> applySubst (Map.fromList [(0, Fix $ TBody TNumber)]) (Fix $ TBody $ TVar 0)
-- Fix (TBody TNumber)
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TBody $ TVar 0)
-- Fix (TRow (TRowEnd Nothing))
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TRow $ TRowEnd $ Just $ RowTVar 0)
-- Fix (TRow (TRowEnd Nothing))
-- >>> applySubst (Map.fromList [(0, Fix $ TRow $ TRowEnd Nothing)]) (Fix $ TRow $ TRowProp "bla" (schemeEmpty $ Fix $ TBody TString) (TRowEnd $ Just $ RowTVar 0))
-- Fix (TRow (TRowProp "bla" (TScheme {schemeVars = [], schemeType = TQual {qualPred = [], qualType = Fix (TBody TString)}}) (TRowEnd Nothing)))
instance Substable Type where
  applySubst :: TSubst -> Type -> Type
  applySubst s ft@(Fix t) =
      case t of
          TBody (TVar n) -> fromMaybe ft $ Map.lookup n s
          TRow l r' -> Fix $ TRow l $ applySubst s r'
          _ -> Fix $ fmap (applySubst s) t

----------------------------------------------------------------------

sortRow :: TRowList t -> TRowList t
sortRow row = row -- TODO implement

data FlatRowEnd t = FlatRowEndTVar (Maybe RowTVar) | FlatRowEndRec TypeId [t]
                  deriving (Eq, Show)

flattenRow :: TRowList t -> (Map TProp (TScheme t), FlatRowEnd t)
flattenRow = flattenRow' (Map.empty, FlatRowEndTVar Nothing)
    where flattenRow' :: (Map TProp (TScheme t), FlatRowEnd t) -> TRowList t -> (Map TProp (TScheme t), FlatRowEnd t)
          flattenRow' (m,r) (TRowProp n t rest) = flattenRow' (Map.insert n t m, r) rest
          flattenRow' (m,_) (TRowEnd r') = (m, FlatRowEndTVar r')
          flattenRow' (m,_) (TRowRec tid ts) = (m, FlatRowEndRec tid ts)

unflattenRow :: Map TProp (TScheme t) -> FlatRowEnd t -> (TProp -> Bool) -> TRowList t
unflattenRow m r f = Map.foldrWithKey (\n t l -> if f n then TRowProp n t l else l) rend m
  where rend = case r of
          FlatRowEndTVar r' -> TRowEnd r'
          FlatRowEndRec tid ts -> TRowRec tid ts

instance Substable (TRowList Type) where
  applySubst s (TRowProp propName propType rest) = sortRow $ TRowProp propName (applySubst s propType) (applySubst s rest)
  applySubst s t@(TRowEnd (Just (RowTVar tvarName))) =
    case Map.lookup tvarName s of
      Nothing -> t
      Just (Fix (TRow _ tRowList)) -> tRowList
      Just (Fix (TCons (TName tid k) ts)) -> TRowRec tid ts
      Just (Fix (TBody (TVar n))) -> TRowEnd $ Just $ RowTVar n
      Just t' -> error $ "Cannot subst row variable into non-row: " ++ show t'
  applySubst _ (TRowEnd Nothing) = TRowEnd Nothing
  applySubst s (TRowRec tid ts) = TRowRec tid $ applySubst s ts

----------------------------------------------------------------------
newtype ClassName = ClassName String
                  deriving (Show, Eq, Ord, Generic)
instance Hashable ClassName where

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
schemeFromQual = TScheme []

type TypeScheme = TScheme Type

instance VarNames t => VarNames (TQual t) where
    freeTypeVars (TQual p t) = freeTypeVars p `Set.union` freeTypeVars t
    mapVarNames f (TQual p t) = TQual (mapVarNames f p) (mapVarNames f t)

-- | Substable instance for TQual
-- >>> let qt = TQual [TPredIsIn (ClassName "Bla") (Fix $ TBody (TVar 0))] (Fix $ TBody (TVar 0))
-- >>> let s = singletonSubst 0 (Fix $ TBody TNumber)
-- >>> applySubst s qt
-- TQual {qualPred = [TPredIsIn {predClass = ClassName "Bla", predType = Fix (TBody TNumber)}], qualType = Fix (TBody TNumber)}
instance (Substable t, VarNames t) => Substable (TQual t) where
    applySubst s (TQual preds t) = TQual { qualPred = filter (not . Set.null . freeTypeVars) (applySubst s preds)
                                         , qualType = applySubst s t }

instance VarNames t => VarNames (TPred t) where
    freeTypeVars (TPredIsIn _ t) = freeTypeVars t
    mapVarNames f (TPredIsIn n t) = TPredIsIn n $ mapVarNames f t

instance Substable t => Substable (TPred t) where
    applySubst s (TPredIsIn n t) = TPredIsIn n $ applySubst s t

-- | VarNames instance for TScheme
-- >>> let sc v t = TScheme v (qualEmpty t)
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
-- >>> freeTypeVars $ TScheme [0, 1] (TQual [TPredIsIn (ClassName "Bla") (Fix $ TBody $ TVar 0)] (Fix $ TBody $ TVar 0))
-- fromList []
-- >>> freeTypeVars $ TScheme [0, 1] (TQual [TPredIsIn (ClassName "Bla") (Fix $ TBody $ TVar 0)] (Fix $ TBody $ TVar 2))
-- fromList [2]
-- >>> freeTypeVars $ TScheme [0, 1] (TQual [TPredIsIn (ClassName "Bla") (Fix $ TBody $ TVar 2)] (Fix $ TBody $ TVar 2))
-- fromList [2]
-- >>> freeTypeVars $ TScheme [0, 1] (TQual [TPredIsIn (ClassName "Bla") (Fix $ TBody $ TVar 2)] (Fix $ TBody $ TVar 0))
-- fromList [2]
instance VarNames t => VarNames (TScheme t) where
  freeTypeVars (TScheme qvars t) = freeTypeVars t `Set.difference` Set.fromList qvars
  mapVarNames f (TScheme qvars t) = TScheme (map f qvars) (mapVarNames f t)

instance (VarNames t, Substable t) => Substable (TScheme t) where
    applySubst = schemeForceApplySubst

-- -- | Substitution on TScheme that doesn't touch quantified variables
-- -- Useful for normal substitution
-- schemeQApplySubst :: (VarNames t, Substable t) => TSubst -> TScheme t -> TScheme t
-- schemeQApplySubst s (TScheme qvars t) = TScheme qvars $ applySubst (foldr Map.delete s qvars) t

-- | Substitution on TScheme that *does* replace even quantified variables
-- Useful for un-generalizing mutable variables
schemeForceApplySubst :: (VarNames t, Substable t) => TSubst -> TScheme t -> TScheme t
schemeForceApplySubst s (TScheme qvars t) = TScheme qvars' t'
    where qvars' = Set.toList $ Set.fromList qvars `Set.intersection` freeTypeVars t'
          t' = applySubst s t


newtype VarId = VarId Int
                deriving (Show, Eq, Ord, Generic)

instance Hashable VarId where

-- | Type environment: maps AST variables (not type variables!) to quantified type schemes.
--
-- Note: instance of Substable
type TypeEnv = Map EVarName VarId

-- Used internally to generate fresh type variable names
data NameSource = NameSource { lastName :: Int }
                deriving (Show, Eq)


data InferState = InferState { nameSource   :: NameSource
                             , mainSubst    :: TSubst
                             -- must be stateful because we sometimes discover that a variable is mutable.
                             , varSchemes   :: Map VarId TypeScheme
                             , namedTypes   :: Map TypeId (Type, TypeScheme)
                             , classes      :: Map ClassName (Class Type)
                             , pendingUni   :: Set.Set (Source, Type, (ClassName, Set.Set TypeScheme))
                             }
                   deriving (Show, Eq)


-- | VarNames instance for InferState
-- >>> :{
-- varInstances
-- $ mapVarNames (\k -> k + 1)
-- $ emptyInferState { varInstances = Map.fromList [ (0, Set.fromList [ qualEmpty $ Fix $ TBody $ TVar 0, qualEmpty $ Fix $ TBody $ TVar 1])
--                                                 , (1, Set.fromList [ qualEmpty $ Fix $ TBody $ TVar 0
--                                                                    , TQual [TPredIsIn (ClassName "Bla") (Fix $ TBody $ TVar 3)] (Fix $ TBody $ TVar 1)
--                                                                    ])
--                                                 ]}
-- :}
-- fromList [(1,fromList [TQual {qualPred = [], qualType = Fix (TBody (TVar 1))},TQual {qualPred = [], qualType = Fix (TBody (TVar 2))}]),(2,fromList [TQual {qualPred = [], qualType = Fix (TBody (TVar 1))},TQual {qualPred = [TPredIsIn {predClass = ClassName "Bla", predType = Fix (TBody (TVar 4))}], qualType = Fix (TBody (TVar 2))}])]
instance VarNames InferState where
  freeTypeVars = freeTypeVars . varSchemes
  mapVarNames f is = is { varSchemes = mapVarNames f $ varSchemes is
                        }

instance Substable InferState where
  applySubst s is = is { varSchemes = applySubst s (varSchemes is)
                       , mainSubst = s `composeSubst` mainSubst is
                       }


----------------------------------------------------------------------

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
