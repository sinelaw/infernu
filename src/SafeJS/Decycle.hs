{-# LANGUAGE ScopedTypeVariables #-}

-- | copied from https://raw.githubusercontent.com/Peaker/lamdu/wip_integration/bottlelib/Data/Function/Decycle.hs

module SafeJS.Decycle(decycleOn, decycle, decycle2, decycle3) where

import Data.Functor((<$>))
import qualified Data.Set as Set

-- | A fix for functions that terminates recursive cycles
decycleOn :: forall a b res . Ord b => (a -> b) -> (Maybe (a -> res) -> a -> res) -> a -> res
decycleOn toOrd f = go Set.empty
  where
    go :: Set.Set b -> a -> res
    go visited x = f (mRecurse visited (toOrd x)) x
    
    mRecurse :: Set.Set b -> b -> Maybe (a -> res)
    mRecurse visited o = if Set.member o visited
                         then Nothing
                         else Just $ go (Set.insert o visited)

decycle :: Ord a => (Maybe (a -> res) -> a -> res) -> a -> res
decycle = decycleOn id

decycle2 :: (Ord b, Ord a) => (Maybe (a -> b -> c) -> a -> b -> c) -> a -> b -> c
decycle2 f arg1 arg2 = decycle (\recurse (arg1', arg2') -> f (curry <$> recurse) arg1' arg2') (arg1, arg2)

decycle3 :: (Ord b, Ord a, Ord c) => (Maybe (a -> b -> c -> res) -> a -> b -> c -> res) -> a -> b -> c -> res
decycle3 f arg1 arg2 arg3 = decycle f3 (arg1, arg2, arg3)
  where f3 Nothing (x,y,z) = f Nothing x y z
        f3 (Just rec3) (x,y,z) = f (Just (\a b c -> rec3 (a,b,c))) x y z
