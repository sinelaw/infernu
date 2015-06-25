{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Date
       (dateRowType)
       where

import           Control.Monad         (foldM)

import           Infernu.Builtins.Util
import           Infernu.InferState    (Infer)
import           Infernu.Prelude
import           Infernu.Types

dateFunc :: Type -> Type
dateFunc = funcN [date]

getNumFromDate :: TScheme Type
getNumFromDate = ty $ dateFunc number

getNumFuncs :: [String]
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

setNumOnDate :: TScheme Type
setNumOnDate = ty $ funcN [date, number] number

-- Note: many of these functions accept multiple optional parameters, but there are other ways to
-- achieve the same result. Leaving these optional parameters out.
setNumFuncs :: [String]
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

getStringFuncType :: TScheme (Fix FType)
getStringFuncType = ty $ funcN [date] string

getStringFuncs :: [String]
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
