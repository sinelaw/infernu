{-# LANGUAGE CPP #-}

-- | The sole purpose of this module is to fix pre/post ghc 7.10 compatibility issues
module Infernu.Prelude
  ( module Prelude.Compat
  , bool
  )
where

import Prelude.Compat
import Data.Orphans ()

#if MIN_VERSION_base(4,7,0)
import Data.Bool (bool)
#else

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t

#endif
