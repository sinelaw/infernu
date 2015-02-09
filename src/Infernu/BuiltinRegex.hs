{-# LANGUAGE TupleSections #-}
module Infernu.BuiltinRegex
       (regexRowType)
       where

import           Control.Monad             (foldM, forM)
import Infernu.Types
import Infernu.InferState
import           Infernu.Lib (safeLookup)

string :: Type
string = Fix $ TBody TString

-- regex :: Type
-- regex = Fix $ TBody TRegex

ts :: t -> TScheme t
ts = TScheme []

regexProps :: [(String, TypeScheme)]
regexProps = 
  [ ("source", ts string)
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
regexRowType :: Infer (TRowList Type)
regexRowType = foldM addProp (TRowEnd Nothing) $ regexProps
  where addProp rowlist (name, propTS) =
          do allocNames <- forM (schemeVars propTS) $ \tvName -> (fresh >>= return . (tvName,))
             let ts' = mapVarNames (safeLookup allocNames) propTS
             return $ TRowProp name ts' rowlist
