{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Builtins.TypeClasses
       (typeClasses)
       where

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
                                               [ TScheme [Flex 0] $ qualEmpty $ Fix $ TRow Nothing $ TRowEnd $ Just $ RowTVar (Flex 0)
                                               , TScheme [Flex 0] $ qualEmpty $ Fix $ TCons TArray [Fix . TBody . TVar $ Flex 0]
                                               , TScheme [Flex 0] $ qualEmpty $ Fix $ TCons TStringMap [Fix . TBody . TVar $ Flex 0]
                                               ]})
    ]
