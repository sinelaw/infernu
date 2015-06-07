{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Date
       (dateRowType)
       where

import           Control.Monad             (foldM, forM)
import Infernu.Types
import Infernu.InferState
import           Infernu.Lib (safeLookup)
import           Infernu.Prelude
import Infernu.Builtins.Util

dateFunc :: Type -> Type
dateFunc x = funcN [date] x

dateProps :: [(String, TypeScheme)]
dateProps =
  [ ("getDate", ty $ dateFunc number)
  , ("getDay", ty $ dateFunc number)
  , ("getFullYear", ty $ dateFunc number)
  , ("getHours", ty $ dateFunc number)
  , ("getMilliseconds", ty $ dateFunc number)
  , ("getMinutes", ty $ dateFunc number)
  , ("getMonth", ty $ dateFunc number)
  , ("getSeconds", ty $ dateFunc number)
  , ("getTime", ty $ dateFunc number)
  , ("getTimezoneOffset", ty $ dateFunc number)
  , ("getUTCDate", ty $ dateFunc number)
  , ("getUTCDay", ty $ dateFunc number)
  , ("getUTCFullYear", ty $ dateFunc number)
  , ("getUTCHours", ty $ dateFunc number)
  , ("getUTCMilliseconds", ty $ dateFunc number)
  , ("getUTCMinutes", ty $ dateFunc number)
  , ("getUTCMonth", ty $ dateFunc number)
  , ("getUTCSeconds", ty $ dateFunc number)
    -- TODO add more...
  ]

dateRowType :: Infer (TRowList Type)
dateRowType = namedProps
  where namedProps = foldM addProp (TRowEnd Nothing) $ dateProps
        addProp rowlist (name, propTS) =
          do allocNames <- forM (schemeVars propTS) $ \tvName -> ((tvName,) . Flex <$> fresh)
             let ts' = mapVarNames (safeLookup allocNames) propTS
             return $ TRowProp (TPropName name) ts' rowlist
