-- |

module Infernu.Source
       ( GenInfo(..)
       , Source(..), SourcePosSpan(..)
       , emptySource
       , TypeError(..)
       ) where

import qualified Text.Parsec.Pos           as Pos
import Text.PrettyPrint.ANSI.Leijen (Doc)

import           Infernu.Prelude

data GenInfo = GenInfo { isGen :: Bool, declName :: Maybe String }
             deriving (Show, Eq, Ord)

data SourcePosSpan = SourcePosSpan Pos.SourcePos Pos.SourcePos | SourcePosGlobal
                   deriving (Show, Eq, Ord)

newtype Source = Source (GenInfo, SourcePosSpan)
               deriving (Show, Eq, Ord)

emptySource :: Source
emptySource = Source (GenInfo True Nothing, SourcePosSpan (Pos.initialPos "") (Pos.initialPos ""))

data TypeError = TypeError { source :: Source, message :: Doc }
               deriving (Show)


