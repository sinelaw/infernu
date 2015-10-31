-- |

module Infernu.Parse.Types where

import Infernu.Parse.Parser as P
import Control.Applicative ((<|>), (*>), many, some)
import Infernu.Types (Type, FType(..))
import           Infernu.Prelude

ident = (P.lower <|> P.is (=='_'))
        *> (many (P.alphaNum <|> P.oneOf "_'"))
