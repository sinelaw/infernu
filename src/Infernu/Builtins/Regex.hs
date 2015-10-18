{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Regex
       (regexRowType)
       where

import           Control.Monad         (foldM)

import           Infernu.Builtins.Util
import           Infernu.InferState    (Infer)
import           Infernu.Prelude
import           Infernu.Types
import           Infernu.Expr       (EPropName(..))

regexMatch :: Type
regexMatch = record (Just "RegexMatch")
             -- TODO: instead of quantifying 'this', it should be a recursive type (regexMatch itself)
             . TRowProp (TPropGetName EPropGetIndex) (ts [] $ func (tvar 0) number string)
             . TRowProp (TPropGetName $ EPropName "index") (ty number)
             . TRowProp (TPropGetName $ EPropName "input") (ty string)
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
