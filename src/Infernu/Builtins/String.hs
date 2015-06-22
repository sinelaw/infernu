{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.String
       (stringRowType)
       where

import           Control.Monad         (foldM, forM)

import           Infernu.Builtins.Util
import           Infernu.InferState    (Infer)
import           Infernu.Lib           (safeLookup)
import           Infernu.Prelude
import           Infernu.Types


stringProps :: [(String, TypeScheme)]
stringProps =
  [ ("length", ty number)
  , ("charAt", ty $ func string number string)
  , ("charCodeAt", ty $ func string number number)
  , ("concat", ty $ func string string string) -- TODO: concat really accepty a variable number of arguments
  , ("indexOf", ty $ func string string number) -- TODO: optional parameter
  , ("lastIndexOf", ty $ func string string number) -- TODO: optional parameter
  , ("localeCompare", ty $ func string string number) -- TODO: optional parameters

-- To support 'match' we need to allow different result types, different for global and non-global
-- regexes.  One possibility is to define two regex types RegexSingle and RegexGlobal and use
-- associated types:
-- class Regex r where
--     type R r = r
--     type M RegexLocal = -- match result type for RegexLocal
--     type M RegexGlobal = [String]
--
--  , ("match", ty $ func string regex

  , ("replace", TScheme [Flex 0] $ withTypeClass "Pattern" (tvar 0) $ funcN [string, tvar 0, string] string)
  , ("split", ty $ func string string (array string) ) -- TODO review
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
-- TODO: this code is actually pure, refactor to pure function and 'return' wrapper.
stringRowType :: Infer (TRowList Type)
stringRowType = TRowProp (TPropGetName EPropGetIndex) (ty $ func string number string) <$> namedProps
  where namedProps = foldM addProp (TRowEnd Nothing) $ stringProps
