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
dateFunc = funcN [date]

getNumFromDate = ty $ dateFunc number

getNumFuncs = [ "getDate"
              , "getDay"
              , "getFullYear"
              , "getHours"
              , "getMilliseconds"
              , "getMinutes"
              , "getMonth"
              , "getSeconds"
              , "getTime"
              , "getTimezoneOffset"
              , "getUTCDate"
              , "getUTCDay"
              , "getUTCFullYear"
              , "getUTCHours"
              , "getUTCMilliseconds"
              , "getUTCMinutes"
              , "getUTCMonth"
              , "getUTCSeconds"
              , "valueOf"
              ]

setNumOnDate = ty $ funcN [date, number] number

-- Note: many of these functions accept multiple optional parameters, but there are other ways to
-- achieve the same result. Leaving these optional parameters out.
setNumFuncs =
    [ "setDate"
    , "setFullYear"
    , "setHours"
    , "setMilliseconds"
    , "setMinutes"
    , "setMonth"
    , "setSeconds"
    , "setTime"
    , "setUTCDate"
    , "setUTCFullYear"
    , "setUTCHours"
    , "setUTCMilliseconds"
    , "setUTCMinutes"
    , "setUTCMonth"
    , "setUTCSeconds"
      --               , "setYear"
    ]

getStringFuncType = ty $ funcN [date] string

getStringFuncs =
    [ "toDateString"
--    , "toGMTString"
    , "toISOString"
    , "toJSON"
--    , "toLocaleFormat"
--    , "toSource"
    , "toString"
    , "toTimeString"
    , "toUTCString"
    ]
    -- , "toLocaleDateString"
    -- , "toLocaleString"
    -- , "toLocaleTimeString"

dateProps :: [(String, TypeScheme)]
dateProps =
    map (,getNumFromDate) getNumFuncs
    ++ map (,setNumOnDate) setNumFuncs
    ++ map (,getStringFuncType) getStringFuncs

dateRowType :: Infer (TRowList Type)
dateRowType = namedProps
  where namedProps = foldM addProp (TRowEnd Nothing) dateProps
        addProp rowlist (name, propTS) =
          do allocNames <- forM (schemeVars propTS) $ \tvName -> (tvName,) . Flex <$> fresh
             let ts' = mapVarNames (safeLookup allocNames) propTS
             return $ TRowProp (TPropName name) ts' rowlist
