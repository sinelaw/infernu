{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Regex
       (regexRowType)
       where

import           Control.Monad         (foldM)

import           Infernu.Builtins.Util
import           Infernu.InferState    (Infer)
import           Infernu.Prelude
import           Infernu.Types

regexMatch :: Type
regexMatch = Fix . TRow (Just "RegexMatch")
             -- TODO: instead of quantifying 'this', it should be a recursive type (regexMatch itself)
             . TRowProp (TPropName EPropGetIndex) (ts [] $ func (tvar 0) number string)
             . TRowProp (TPropName $ EPropGetName "index") (ty number)
             . TRowProp (TPropName $ EPropGetName "input") (ty string)
             $ TRowEnd Nothing

regexProps :: [(String, TypeScheme)]
regexProps =
  [ ("source", ty string)
  , ("exec", ty $ func regex string regexMatch)
  , ("lastIndex", ty number)
  , ("global", ty boolean)
  , ("ignoreCase", ty boolean)
  , ("multiline", ty boolean)
  , ("source", ty string)
  , ("test", ty $ func regex string boolean)
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
-- TODO: this code is actually pure, refactor to pure function and 'return' wrapper.
regexRowType :: Infer (TRowList Type)
regexRowType = foldM addProp (TRowEnd Nothing) $ regexProps
