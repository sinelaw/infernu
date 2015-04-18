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
       (GenInfo(..)
       , Source(..)
       , emptySource
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
       , mapTopAnnotation
#ifdef QUICKCHECK
       , runAllTests
#endif
       ) where

import qualified Data.Map.Lazy             as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set
import qualified Data.Graph.Inductive      as Graph
import qualified Text.Parsec.Pos           as Pos

import           Infernu.Fix               (Fix (..), replaceFix)
import           Infernu.Prelude
import Prelude ()

#ifdef QUICKCHECK
import           Data.DeriveTH
import           Data.Map.Lazy             (Map)
import qualified Data.Map.Lazy             as Map
import           Test.QuickCheck           (choose, resize)
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
#endif

data GenInfo = GenInfo { isGen :: Bool, declName :: Maybe String }
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
           | ECase a (Exp a) [(LitVal, Exp a)]
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

data TConsName = TArray | TTuple | TName TypeId | TStringMap
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
               -- | TFunc (functions) are Profunctor-types. Arguments could have been a single 't'
               -- and always wrapped in a Tuple - but are expanded to a list here for convenience
             | TFunc [t] t 
             | TRow (TRowList t)
               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Type = Fix FType

newtype Source = Source (GenInfo, Pos.SourcePos)
               deriving (Show, Eq, Ord)

emptySource :: Source
emptySource = Source (GenInfo True Nothing, Pos.initialPos "")
               
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
  mapVarNames f = f

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
-- >>> freeTypeVars (Fix $ TFunc [Fix $ TBody $ TVar 0] (Fix $ TBody $ TVar 1))
-- fromList [0,1]
-- >>> freeTypeVars (Fix $ TFunc [] (Fix $ TBody $ TVar 1))
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

type TSubst = Map.Map TVarName Type

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
-- Fix (TRow (TRowProp "bla" (TScheme {schemeVars = [], schemeType = TQual {qualPred = [], qualType = Fix (TBody TString)}}) (TRowEnd Nothing)))
instance Substable Type where
  applySubst :: TSubst -> Type -> Type
  applySubst s ft@(Fix t) =
    case t of
     TBody (TVar n) -> substT' n t
     TRow r -> Fix $ TRow $ applySubst s r
     _ -> if ft `elem` Map.elems s
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
    applySubst s (TQual preds t) = TQual (applySubst s preds) (applySubst s t)

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

-- | Substitution on TScheme that doesn't touch quantified variables
-- Useful for normal substitution
schemeQApplySubst :: (VarNames t, Substable t) => TSubst -> TScheme t -> TScheme t
schemeQApplySubst s (TScheme qvars t) = TScheme qvars $ applySubst (foldr Map.delete s qvars) t

-- | Substitution on TScheme that *does* replace even quantified variables
-- Useful for un-generalizing mutable variables
schemeForceApplySubst :: (VarNames t, Substable t) => TSubst -> TScheme t -> TScheme t
schemeForceApplySubst s (TScheme qvars t) = TScheme qvars' t'
    where qvars' = Set.toList $ Set.fromList qvars `Set.intersection` freeTypeVars t'
          t' = applySubst s t


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
                             , varInstances :: Graph.Gr QualType ()
                             , namedTypes   :: Map.Map TypeId (Type, TypeScheme)
                             , classes      :: Map.Map ClassName (Class Type)
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
                        , varInstances = Graph.nmap (mapVarNames f) $ varInstances is
                        }

instance Substable InferState where
  applySubst s is = is { varSchemes = applySubst s (varSchemes is)
                       , mainSubst = s `composeSubst` mainSubst is
                       , varInstances = Graph.nmap (applySubst s)  $ varInstances is
                       }
            
-- | Adds a pair of equivalent items to an equivalence map.
-- >>> import Infernu.Pretty
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
addEquivalence :: TVarName -> TVarName -> Graph.Gr QualType () -> Graph.Gr QualType ()
addEquivalence x y gr = Graph.insEdge (x,y,()) . insTVar x . insTVar y $ gr
    where insTVar tv g = if Graph.gelem tv g
                         then g
                         else Graph.insNode (tv, qualEmpty $ Fix . TBody $ TVar tv) g


----------------------------------------------------------------------
-- TODO: Horrible, terrible boilerplate. get rid of it.
mapTopAnnotation :: (a -> a) -> Exp a -> Exp a
mapTopAnnotation f expr =
    case expr of
        (EVar a b) -> EVar (f a) b
        (EApp a x y) -> EApp (f a) x y
        (EAbs a x y) -> EAbs (f a) x y
        (ELet a x y z) -> ELet (f a) x y z
        (ELit a x) -> ELit (f a) x
        (EAssign a x y z) -> EAssign (f a) x y z
        (EPropAssign a x y z v) -> EPropAssign (f a) x y z v
        (EIndexAssign a x y z v) -> EIndexAssign (f a) x y z v
        (EArray a x) -> EArray (f a) x
        (ETuple a x) -> ETuple (f a) x
        (ERow a x y) -> ERow (f a) x y
        (EStringMap a x) -> EStringMap (f a) x
        (ECase a x ys) -> ECase (f a) x ys
        (EProp a x y) -> EProp (f a) x y
        (EIndex a x y) -> EIndex (f a) x y
        (ENew a x y) -> ENew (f a) x y

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
