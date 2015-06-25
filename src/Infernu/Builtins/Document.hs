{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Document
       where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types


elementGlobalAttributes :: [(String, TypeScheme)]
elementGlobalAttributes =
    -- TODO add more
  [ ("accesskey",  ty string)
  , ("class",      ty string)
  , ("id",         ty string)
  , ("style",      ty string)
  , ("tabindex",   ty number)
  , ("title",      ty string)
  ]

