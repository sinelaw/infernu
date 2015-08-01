module Infernu.Builtins
       ( builtins
       , arrayRowType
       , dateRowType
       , regexRowType
       , stringRowType
       , stringMapRowType
       )
       where

import           Infernu.Builtins.Array     (arrayRowType)
import           Infernu.Builtins.Date      (dateRowType)
import qualified Infernu.Builtins.Operators as Operators
import           Infernu.Types              (EVarName, TypeScheme)
import           Infernu.Builtins.Regex     (regexRowType)
import           Infernu.Builtins.String    (stringRowType)
import           Infernu.Builtins.StringMap (stringMapRowType)

import           Data.Map                   (Map)

builtins :: Map EVarName TypeScheme
builtins = Operators.builtins

