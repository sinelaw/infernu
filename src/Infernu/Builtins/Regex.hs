{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Regex
       (regexRowType)
       where

import           Control.Monad             (foldM, forM)
import Infernu.Types
import Infernu.InferState
import           Infernu.Lib (safeLookup)
import Infernu.Prelude
import Infernu.Builtins.Util
       
regexMatch :: Type
regexMatch = Fix . TRow (Just "RegexMatch")
             -- TODO: instead of quantifying 'this', it should be a recursive type (regexMatch itself)
             . TRowProp (TPropGetIndex) (ts [0] $ func (tvar 0) number $ maybeT string)
             . TRowProp (TPropName "index") (ty number)
             . TRowProp (TPropName "input") (ty string)
             $ TRowEnd Nothing

regexProps :: [(String, TypeScheme)]
regexProps = 
  [ ("source", ty string)
  , ("exec", ty $ func regex string regexMatch)
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
-- TODO: this code is actually pure, refactor to pure function and 'return' wrapper.
regexRowType :: Infer (TRowList Type)
regexRowType = foldM addProp (TRowEnd Nothing) $ regexProps
  where addProp rowlist (name, propTS) =
          do allocNames <- forM (schemeVars propTS) $ \tvName -> ((tvName,) . Flex <$> fresh)
             let ts' = mapVarNames (safeLookup allocNames) propTS
             return $ TRowProp (TPropName name) ts' rowlist
