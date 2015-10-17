module Infernu.Builtins.TypeClasses
       (typeClasses)
       where

import           Infernu.Prelude
import           Infernu.Types

tcons :: TConsName -> [Type] -> FType Type
tcons n ts = TCons n ts

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
                                               [ TScheme [Flex 0 KStar] $ qualEmpty $ Fix $ TRow Nothing $ TRowEnd $ Just $ RowTVar (Flex 0 KStar)
                                               , TScheme [Flex 0 KStar] $ qualEmpty $ Fix $ tcons TArray [Fix . TBody $ TVar (Flex 0 KStar)]
                                               , TScheme [Flex 0 KStar] $ qualEmpty $ Fix $ tcons TStringMap [Fix . TBody $ TVar (Flex 0 KStar)]
                                               ]})
    ]
