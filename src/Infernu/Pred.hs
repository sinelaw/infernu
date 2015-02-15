{-# LANGUAGE TupleSections #-}
module Infernu.Pred
       (unify, mkAnd, mkOr)
    where


import           Data.Traversable (sequenceA)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe    (catMaybes)
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Infernu.Types (TPred (..), TVarName)

data CanonPredOr t = CanonPredOr [Map TVarName (Set t)]
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

mkAnd :: Eq t => TPred t -> TPred t -> TPred t
mkAnd TPredTrue y = y
mkAnd x TPredTrue = x
mkAnd x y = if x == y then x else TPredAnd x y


mkOr :: Eq t => TPred t -> TPred t -> TPred t
mkOr TPredTrue y = y
mkOr x TPredTrue = x
mkOr x y = if x == y then x else TPredOr x y

-- | Converts a predicate to a list of sums of products
--
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
-- >>> fromCanon $ CanonPredOr [Map.fromList [(0,Set.fromList "ac")],Map.fromList [(0,Set.fromList "a"),(1,Set.fromList "d")],Map.fromList [(0,Set.fromList "c"),(1,Set.fromList "b")],Map.fromList [(1,Set.fromList "bd")]]
-- TPredOr (TPredAnd (TPredEq 1 'd') (TPredEq 0 'a')) (TPredOr (TPredAnd (TPredEq 1 'b') (TPredEq 0 'c')) (TPredOr (TPredAnd (TPredEq 1 'd') (TPredEq 1 'b')) (TPredAnd (TPredEq 0 'c') (TPredEq 0 'a'))))
fromCanon :: Eq t => CanonPredOr t -> TPred t
fromCanon (CanonPredOr []) = TPredTrue
fromCanon (CanonPredOr (p:ps)) = foldr mkOr (fromCanonAnd p) $ map fromCanonAnd ps
    where fromCanonAnd m = Map.foldrWithKey (\k vs r -> Set.foldr (\v p' -> mkAnd p' $ TPredEq k v) r vs) TPredTrue m

-- | Main unification function: checks if two predicates can be combined in a conjunction (i.e. if they can ANDed)
-- >>> unify (==) (TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')) (TPredEq 0 'a')
-- Just (TPredAnd (TPredEq 1 'b') (TPredEq 0 'a'))
-- >>> unify (==) (TPredOr (TPredEq 0 'a') (TPredEq 1 'b')) (TPredEq 0 'a')
-- Just (TPredOr (TPredAnd (TPredEq 1 'b') (TPredEq 0 'a')) (TPredEq 0 'a'))
unify :: (Applicative f, Ord t) => (t -> t -> f ()) -> TPred t -> TPred t -> f (TPred t)
unify u p1 p2 = fmap fromCanon $ unifyPreds u (toCanon p1) (toCanon p2)

unifyPreds :: (Applicative f, Ord t) => (t -> t -> f ()) -> CanonPredOr t -> CanonPredOr t -> f (CanonPredOr t)
unifyPreds u (CanonPredOr m1s) (CanonPredOr m2s) =
    CanonPredOr <$> sequenceA [unifyMaps u m1 m2 | m1 <- m1s, m2 <- m2s]

-- TODO: REfactor    
-- Map a   mismatching keys -> survive the key as is
--     b   matching keys -> unify the values, fail delete keys
--     
-- typevar => Type          typevar => Type 
--       a => Number   &    a = Int
--       b => String   &    b = Number
--       c => Number

-- Get rid of Set - do the unification at the toCanon level
-- Use newtypes
-- Use effectful unifyWith (using pure to lift pure values in the map to f a)
      
--    (╯°□°）╯︵ ┻━┻
unifyMaps ::
    (Applicative f, Ord t, Ord k)
    => (t -> t -> f ())
    -> Map k (Set t)
    -> Map k (Set t)
    -> f (Map k (Set t))
unifyMaps u m1 m2 = 
unifyMaps u m1 m2 = Map.traverseWithKey (\_ (isGood, s) -> if isGood then Just s else Nothing)
                    $ Map.mergeWithKey (\_ t1 t2 -> Just (unifySets u t1 t2, Set.union t1 t2)) (Map.map (True,)) (Map.map (True,)) m1 m2

unifySets :: (Applicative f, Ord t) => (t -> t -> f ()) -> Set t -> Set t -> f ()
unifySets u s1 s2 = sequenceA_ [x `u` y | x <- Set.toList s1, y <- Set.toList s2]

