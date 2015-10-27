-- |

module Infernu.Source
       ( GenInfo(..)
       , Source(..), SourcePosSpan(..), Comment(..)
       , emptySource
       , TypeError(..)
       ) where

import qualified Text.Parsec.Pos           as Pos
import Text.PrettyPrint.ANSI.Leijen (Doc)

import           Infernu.Prelude

data Comment = SingleLineComment String
             | MultiLineComment String
             deriving (Show, Eq)

data GenInfo = GenInfo { isGen :: Bool, declName :: Maybe String }
             deriving (Show, Eq, Ord)

data SourcePosSpan = SourcePosSpan Pos.SourcePos Pos.SourcePos | SourcePosGlobal
                   deriving (Show, Eq, Ord)

data Source = Source { srcGenInfo :: GenInfo, srcPosSpan :: SourcePosSpan }
            deriving (Show, Eq, Ord)

emptySource :: Source
emptySource = Source { srcGenInfo = GenInfo True Nothing
                     , srcPosSpan = SourcePosGlobal
                     }

data TypeError = TypeError { source :: Source, message :: Doc }
               deriving (Show)


