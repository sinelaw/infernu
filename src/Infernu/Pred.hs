{-# LANGUAGE TupleSections #-}
module Infernu.Pred
--      (unify, mkAnd, mkOr)
    where

import           Control.Applicative (Applicative (..), (*>), (<$>))
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (catMaybes)
import           Data.Traversable    (sequenceA)

import           Data.Set            (Set)
import qualified Data.Set            as Set
    
import           Infernu.Types       (TPred (..), TVarName, mkAnd, mkOr)
import           Infernu.Decycle     (decycle)
    
data CanonPredOr t = CanonPredOr { canonPredAnds :: [Map TVarName (Set t)] }
                     deriving (Show, Eq)

-- | Converts a predicate to DNF (Disjunction Normal Form)
-- based on code from the hatt package
--
-- >>> toDNF $ TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')
-- TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')
-- >>> toDNF $ TPredOr (TPredEq 0 'a') (TPredEq 1 'b')
-- TPredOr (TPredEq 0 'a') (TPredEq 1 'b')
-- >>> let or' = TPredOr (TPredEq 0 'a') (TPredEq 1 'b')
-- >>> toDNF $ TPredAnd or' or'
-- TPredOr (TPredEq 0 'a') (TPredEq 1 'b')
-- >>> toDNF $ TPredAnd (TPredOr (TPredEq 0 'a') (TPredEq 1 'b')) (TPredOr (TPredEq 0 'c') (TPredEq 1 'd'))
-- TPredOr (TPredOr (TPredAnd (TPredEq 0 'a') (TPredEq 0 'c')) (TPredAnd (TPredEq 0 'a') (TPredEq 1 'd'))) (TPredOr (TPredAnd (TPredEq 1 'b') (TPredEq 0 'c')) (TPredAnd (TPredEq 1 'b') (TPredEq 1 'd')))
toDNF :: Eq t => TPred t -> TPred t
toDNF (TPredAnd exp1 exp2) = if exp1 == exp2
                             then toDNF exp1
                             else toDNF exp1 `dist` toDNF exp2
    where
        dist (TPredOr e11 e12) e2 = (e11 `dist` e2) `mkOr` (e12 `dist` e2)
        dist e1 (TPredOr e21 e22) = (e1 `dist` e21) `mkOr` (e1 `dist` e22)
        dist e1 e2                = if e1 == e2 then e1 else e1 `mkAnd` e2
toDNF (TPredOr exp1 exp2) = toDNF exp1 `mkOr` toDNF exp2
toDNF expr                    = expr

data CanonPredSplit t = CanonPredSplit { canonAmbiguousPreds :: [Map TVarName (Set t)]
                                       , canonUnambiguousPreds :: Map TVarName (Set t) }

splitCanon :: Ord t => CanonPredOr t -> CanonPredSplit t
splitCanon (CanonPredOr ands') = CanonPredSplit { canonAmbiguousPreds = ambigs, canonUnambiguousPreds = intersections }
    where intersections = Map.unionsWith Set.intersection ands'
          ambigs = map (Map.mapWithKey (\k vs -> Set.difference vs $ Map.findWithDefault Set.empty k intersections)) ands'
    
                                
-- | Converts a predicate to a list of sums of products
--
-- >>> toCanon $ TPredAnd (TPredAnd (TPredEq 0 'a') (TPredEq 0 'b')) (TPredAnd (TPredEq 0 'c') (TPredEq 0 'd'))
-- CanonPredOr [fromList [(0,fromList "abcd")]]
-- >>> toCanon $ TPredAnd (TPredOr (TPredEq 0 'a') (TPredEq 1 'b')) (TPredOr (TPredEq 0 'c') (TPredEq 1 'd'))
-- CanonPredOr [fromList [(0,fromList "ac")],fromList [(0,fromList "a"),(1,fromList "d")],fromList [(0,fromList "c"),(1,fromList "b")],fromList [(1,fromList "bd")]]
toCanon :: (Ord t, Eq t) => TPred t -> CanonPredOr t
toCanon = CanonPredOr . toCanonOr' . toDNF
    where toCanonOr' (TPredOr p1 p2) = toCanonOr' p1 ++ toCanonOr' p2
          toCanonOr' p = [toCanonAnd' p]

          toCanonAnd' (TPredOr _ _) = error "toDNF didn't supply DNF..."
          toCanonAnd' (TPredAnd p1 p2) = Map.unionWith Set.union (toCanonAnd' p1) (toCanonAnd' p2)
          toCanonAnd' (TPredEq v t) = Map.singleton v (Set.singleton t)
          toCanonAnd' TPredTrue = Map.empty

-- |
-- >>> fromCanon $ CanonPredOr [Map.fromList [(0,Set.fromList "abcd")]]
-- TPredAnd (TPredEq 0 'a') (TPredAnd (TPredEq 0 'b') (TPredAnd (TPredEq 0 'c') (TPredEq 0 'd')))
-- >>> fromCanon $ CanonPredOr [Map.fromList [(0,Set.fromList "ac")],Map.fromList [(0,Set.fromList "a"),(1,Set.fromList "d")],Map.fromList [(0,Set.fromList "c"),(1,Set.fromList "b")],Map.fromList [(1,Set.fromList "bd")]]
-- TPredOr (TPredAnd (TPredEq 0 'a') (TPredEq 1 'd')) (TPredOr (TPredAnd (TPredEq 0 'c') (TPredEq 1 'b')) (TPredOr (TPredAnd (TPredEq 1 'b') (TPredEq 1 'd')) (TPredAnd (TPredEq 0 'a') (TPredEq 0 'c'))))
fromCanon :: (Ord t, Eq t) => CanonPredOr t -> TPred t
fromCanon (CanonPredOr []) = TPredTrue
fromCanon (CanonPredOr ps) = foldr mkOr TPredTrue $ Set.toList $ Set.fromList $ map fromCanonAnd ps
    where fromCanonAnd m = Map.foldrWithKey (\k vs r -> Set.foldr (\v p' -> TPredEq k v `mkAnd` p') r vs) TPredTrue m

-- | Main unification function: checks if two predicates can be combined in a conjunction (i.e. if they can ANDed)
-- >>> let u ps = sequenceA [if x == y then Just () else Nothing | x <- Set.toList ps, y <- Set.toList ps]
-- >>> unify u (TPredEq 0 'a') (TPredEq 0 'b')
-- Nothing
-- >>> unify u (TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')) (TPredEq 0 'b')
-- Nothing
-- >>> unify u (TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')) (TPredEq 0 'a')
-- Just [TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')]
-- >>> unify u (TPredOr (TPredEq 0 'a') (TPredEq 0 'b')) (TPredEq 0 'a')
-- Just [TPredEq 0 'a']
-- >>> unify u (TPredOr (TPredEq 0 'a') (TPredEq 0 'b')) (TPredOr (TPredEq 0 'a') (TPredEq 0 'c'))
-- Just [TPredEq 0 'a']
-- >>> unify u (TPredOr (TPredEq 0 'a') (TPredEq 1 'b')) (TPredEq 0 'a')
-- Just [TPredOr (TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')) (TPredEq 0 'a')]
-- >>> unify u (TPredOr (TPredEq 0 'a') (TPredEq 1 'b')) (TPredEq 0 'c')
-- Just [TPredAnd (TPredEq 0 'c') (TPredEq 1 'b')]
unify :: (Applicative f, Ord t) => (Map TVarName (Set t) -> Maybe (f (Map TVarName (Set t)))) -> TPred t -> TPred t -> Maybe (f (TPred t))
unify u p1 p2 = (fmap . fmap) fromCanon $ unifyPreds u (toCanon p1) (toCanon p2)

unifyPreds :: (Applicative f, Ord t) => (Map TVarName (Set t) -> Maybe (f (Map TVarName (Set t)))) -> CanonPredOr t -> CanonPredOr t -> Maybe (f (CanonPredOr t))
unifyPreds u (CanonPredOr m1s) (CanonPredOr m2s) =
    case survivors of
        [] -> Nothing
        preds -> Just $ (CanonPredOr <$> sequenceA preds)
    where survivors = catMaybes $ [unifyMaps u m1 m2 | m1 <- m1s, m2 <- m2s]

-- TODO: Refactor
-- Get rid of Set - do the unification at the toCanon level
-- Use newtypes
-- Use effectful unifyWith (using pure to lift pure values in the map to f a)
--    (╯°□°）╯︵ ┻━┻
unifyMaps
  :: (Applicative f, Ord a) =>
     (Map TVarName (Set a) -> Maybe (f (Map TVarName (Set a))))
     -> Map TVarName (Set a)
     -> Map TVarName (Set a)
     -> Maybe (f (Map TVarName (Set a)))
unifyMaps u m1 m2 =  (fmap Map.unions) . sequenceA <$> sequenceA [intersection', diff1, diff2]
    where intersection = Map.intersectionWith Set.union m1 m2
          intersection' = unifySets' intersection
          diff1 = unifySets' $ Map.difference m1 m2
          diff2 = unifySets' $ Map.difference m2 m1
          unifySets' s = (*> pure s) <$> u s
                  

simplify :: (Ord t, Eq t) => Maybe (TPred t -> TPred t) -> TPred t -> TPred t
simplify Nothing p = p
-- (a & b) | (a & c) | d <-> (a & (b | c)) | d                              
simplify (Just r) (TPredOr p1@(TPredAnd a1 b1) p2@(TPredAnd a2 b2)) = r result
    where
        p1' = r p1
        p2' = r p2
        a1' = r a1
        a2' = r a2
        b1' = r b1
        b2' = r b2
        result =
            if p1' == p2'
            then p1'
            else if a1' == a2'
                 then a1' `mkAnd` (b1' `mkOr` b2')
                 else if b1' == b2'
                      then b1' `mkAnd` (a1' `mkOr` a2')
                      else (a1' `mkAnd` b1') `mkOr` (a2' `mkAnd` b2') 
simplify (Just r) (TPredOr p1 p2) =
    let p1' = r p1
        p2' = r p2
    in r $ p1' `mkOr` p2'
simplify (Just r) (TPredAnd p1 p2) =
    let p1' = r p1
        p2' = r p2
    in r $ p1' `mkAnd` p2'
simplify _ p = p

fixSimplify :: (Ord t, Eq t) => TPred t -> TPred t
fixSimplify = decycle simplify
