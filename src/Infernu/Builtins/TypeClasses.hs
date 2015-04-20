module Infernu.Builtins.TypeClasses
       (typeClasses)
       where

import           Infernu.Types

typeClasses =
    [
      (ClassName "Plus", Class { classInstances =
                                         [ schemeEmpty $ Fix $ TBody TNumber
                                         , schemeEmpty $ Fix $ TBody TString
                                         ]})
    ]
