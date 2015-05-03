{-# LANGUAGE CPP #-}

-- | The sole purpose of this module is to fix pre/post ghc 7.10 compatibility issues
module Infernu.Prelude 
  ( module Prelude
#if MIN_VERSION_base(4,8,0)
#else
  , module Data.Functor
  , module Data.Foldable
  , module Data.Monoid
  , module Control.Applicative
  , module Data.Traversable
#endif
#if MIN_VERSION_base(4,7,0)
  , module Data.Bool
#else
  , bool
#endif
  )
where

import Data.Orphans ()
 
#if MIN_VERSION_base(4,8,0)
import Prelude
#else
import Data.Functor ((<$>))
import Data.Foldable         (Foldable (..), foldr)
import Data.Monoid           (Monoid (..))
import Control.Applicative   (Applicative(..))
import Data.Traversable      (Traversable (..))

import Prelude hiding (foldl, foldl1, foldr1, foldr, mapM, sequence)
#endif
    
#if MIN_VERSION_base(4,7,0)
import Data.Bool (bool)
#else

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t

#endif

