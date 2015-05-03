module Infernu.Builtins.TypeClasses
       (typeClasses)
       where

import           Infernu.Types
import           Infernu.Prelude

typeClasses :: [(ClassName, Class (Fix FType))]
typeClasses =
    [
      (ClassName "Pattern", Class { classInstances =
                                    [ schemeEmpty $ Fix $ TBody TRegex
                                    , schemeEmpty $ Fix $ TBody TString
                                    ]})
    , (ClassName "Plus", Class { classInstances =
                                         [ schemeEmpty $ Fix $ TBody TNumber
                                         , schemeEmpty $ Fix $ TBody TString
                                         ]})
    ]
