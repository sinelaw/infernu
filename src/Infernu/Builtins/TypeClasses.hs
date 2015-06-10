module Infernu.Builtins.TypeClasses
       (typeClasses)
       where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types

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
    , (ClassName "StringKeys", Class { classInstances =
                                               [ ts [0] $ openRow 0
                                               , ts [0] $ array (tvar 0)
                                               , ts [0] $ stringMap (tvar 0)
                                               ]})
    ]
