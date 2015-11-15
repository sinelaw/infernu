{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Number
       (numberRowType)
       where

import           Control.Monad         (foldM)

import           Infernu.Builtins.Util
import           Infernu.InferState    (Infer)
import           Infernu.Prelude
import           Infernu.Types
import           Infernu.Expr       (EPropName(..))


numberProps :: [(String, TypeScheme)]
numberProps =
  [
    ("toFixed",      ty $ func number number string)
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
-- TODO: this code is actually pure, refactor to pure function and 'return' wrapper.
numberRowType :: Infer (TRowList Type)
numberRowType = foldM addProp (TRowEnd Nothing) $ numberProps
