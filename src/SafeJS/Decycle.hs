-- | copied from https://raw.githubusercontent.com/Peaker/lamdu/wip_integration/bottlelib/Data/Function/Decycle.hs

module SafeJS.Decycle(decycleOn, decycle, decycle2, decycle3) where

import Data.Functor((<$>))
import Control.Lens.Operators
import qualified Control.Lens as Lens
import qualified Data.Set as Set

-- | A fix for functions that terminates recursive cycles
decycleOn :: Ord b => (a -> b) -> (Maybe (a -> res) -> a -> res) -> a -> res
decycleOn toOrd f =
  go Set.empty
  where
    go visited x = f (mRecurse visited (toOrd x)) x
    mRecurse visited o
      | visited ^. Lens.contains o = Nothing
      | otherwise = visited & Lens.contains o .~ True & go & Just

decycle :: Ord a => (Maybe (a -> res) -> a -> res) -> a -> res
decycle = decycleOn id

decycle2 :: (Ord b, Ord a) => (Maybe (a -> b -> c) -> a -> b -> c) -> a -> b -> c
decycle2 f arg1 arg2 = decycle (\recurse (arg1', arg2') -> f (curry <$> recurse) arg1' arg2') (arg1, arg2)

curry3 f (a,b,c) = f a b c

-- recurse3 :: (Maybe ((a, b, c) -> res)) -> Maybe (a->b->c -> res)
-- recurse3 Nothing  = Nothing
-- recurse3 (Just r) = Just $ (\a b c -> r (a, b, c))

decycle3 :: (Ord b, Ord a, Ord c) => (Maybe (a -> b -> c -> res) -> a -> b -> c -> res) -> a -> b -> c -> res
decycle3 f arg1 arg2 arg3 = decycle f3 (arg1, arg2, arg3)
  where f3 Nothing (x,y,z) = f Nothing x y z
        f3 (Just rec3) (x,y,z) = f (Just (\a b c -> rec3 (a,b,c))) x y z
