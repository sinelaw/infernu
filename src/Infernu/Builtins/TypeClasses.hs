module Infernu.Builtins.TypeClasses
       (typeClasses)
       where

import           Infernu.Prelude
import           Infernu.Types

tcons :: TConsName -> Kind -> [Type] -> FType Type
tcons n k ts = TApp (TCons n k') ts
    where
        k' = karrow k (map kind ts)

typeClasses :: [(ClassName, Class (Fix FType))]
typeClasses =
    [
      (ClassName "Pattern",
       Class
       { classInstances =
         [ schemeEmpty $ Fix $ TBody TRegex
         , schemeEmpty $ Fix $ TBody TString
         ]})
    , (ClassName "Plus",
       Class
       { classInstances =
         [ schemeEmpty $ Fix $ TBody TNumber
         , schemeEmpty $ Fix $ TBody TString
         ]})
    , (ClassName "StringKeys",
       Class
       { classInstances =
         [ TScheme [Flex 0 KStar] $ qualEmpty $ record Nothing $ TRowEnd $ Just $ RowTVar (Flex 0 KStar)
         , TScheme [Flex 0 KStar] $ qualEmpty $ Fix $ tcons TArray KStar [Fix . TBody $ TVar (Flex 0 KStar)]
         , TScheme [Flex 0 KStar] $ qualEmpty $ Fix $ tcons TStringMap KStar [Fix . TBody $ TVar (Flex 0 KStar)]
         ]})
    ]
