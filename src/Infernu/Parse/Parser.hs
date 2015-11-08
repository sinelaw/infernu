{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A simple applicative parser
module Infernu.Parse.Parser where

import Infernu.Decycle (decycle2)
import           Data.Monoid ((<>), mconcat)
import qualified Data.Char as Char
import qualified Data.Char as Char
import qualified Data.Map as Map
import           Data.Map (Map)
import           Infernu.Prelude


type Stream a = [a]

emptyStream :: [a]
emptyStream = []

data PElem a b = PElem (Map a b) -- matching characters a map to tokens b
             deriving (Eq, Ord, Show)

-- runPElem :: Eq s => PElem s -> Stream s -> Maybe (Stream s, s)
runPElem _ [] = Nothing
runPElem (PElem m) (s:ss) = fmap (ss,) $ Map.lookup s m

class Opaque f where
    infixl ^|
    (^|) :: f a -> f a -> f a
    none :: f a

data Parser s a where
    PZero :: Parser s a
    POneOf  :: PElem s a -> Parser s a
    PAlt  :: [Parser s a] -> Parser s a
    POneOrMore :: Parser s a -> Parser s a
    PSeq  :: [Parser s a] -> Parser s a

deriving instance (Show s, Show a) => Show (Parser s a)

instance Opaque (Parser s) where
    none     = PZero
    p1 ^| p2 = PAlt [p1, p2]

oneOrMore p  = POneOrMore p
zeroOrMore p = none ^| oneOrMore p

instance Monoid (Parser s a) where
    mempty        = PSeq []
    x `mappend` y = PSeq [y,x]

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

type RunParser s a = (Parser s a -> Stream s -> [(Stream s, [a])])

runParserSome :: RunParser s a -> Parser s a -> Stream s -> [(Stream s, [a])]
runParserSome r p s = do
    (s', t) <- r p s
    res <- (s', []) : runParserSome r p s'
    return $ fmapParserResult (t++) res

runParserAlt :: RunParser s a ->  [Parser s a] -> Stream s -> [(Stream s, [a])]
runParserAlt r ps s = concat $ map (flip r s) ps

runParserSeq :: RunParser s a -> [Parser s a] -> Stream s -> [(Stream s, [a])]
runParserSeq r ps stream = foldr flattenSeq [(stream, mempty)] ps
    where
        -- flattenSeq :: Parser s a -> [(Stream s, a)] -> [(Stream s, a)]
        flattenSeq p alts = do
            (s, t) <- alts
            (s', t') <- r p s
            return (s', t <> t')

runParserOneOf :: Ord s => PElem s a -> Stream s -> [(Stream s, [a])]
runParserOneOf p s =
    case runPElem p s of
    Nothing -> []
    Just (s', t) -> [(s', [t])]

runParser'' :: Ord s => RunParser s a -> Parser s a -> Stream s -> [(Stream s, [a])]
--runParser'' _  _    [] = []
runParser'' _  PZero _ = []
runParser'' _ (POneOf p) s = runParserOneOf p s
runParser'' r (POneOrMore p) s = runParserSome r p s
runParser'' r (PAlt ps) s = runParserAlt r ps s
runParser'' r (PSeq ps) s = runParserSeq r ps s

runParser' :: Ord s => (Stream s) -> Parser s a -> Stream s -> [(Stream s, [a])]
runParser' [] _ _     = [] -- stream exhausted
runParser' (_:ss) p s = runParser'' (runParser' ss) p s

runParser :: Ord s => Parser s a -> Stream s -> [(Stream s, [a])]
runParser p [] = []
runParser p (s:ss) = runParser' (s:s:ss) p (s:ss)

----------------------------------------------------------------------

is :: Ord s => Map s a -> Parser s a
is = POneOf . PElem

are :: Ord s => Map s a -> Parser s a
are = zeroOrMore . is

allChars = map Char.chr [0..127]
isChar f = is $ foldr (\k -> Map.insert k k) Map.empty $ filter f allChars
lower = isChar Char.isLower
digit = isChar Char.isDigit
upper = isChar Char.isUpper
letter = isChar Char.isLetter
alphaNum = isChar Char.isAlphaNum

str = oneOrMore letter

space = isChar Char.isSpace

openPar = isChar (=='(')
closePar = isChar (==')')

-- withParens :: Parser Char a -> Parser Char a
-- withParens x = openPar *> x <* closePar

-- optParens :: Parser Char a -> Parser Char a
-- optParens x = x ^| withParens x


