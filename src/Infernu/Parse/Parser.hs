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

import           Infernu.Prelude

type Stream a = [a]

emptyStream :: [a]
emptyStream = []

data PElem a = PElem [a] -- list of matching characters
             deriving (Eq, Ord, Show)

-- runPElem :: Eq s => PElem s -> Stream s -> Maybe (Stream s, s)
runPElem _ [] = Nothing
runPElem (PElem cs) (s:ss)
    | s `elem` cs = Just (ss, s)
    | otherwise   = Nothing

class Opaque f where
    infixl ^|
    (^|) :: f a -> f a -> f a
    none :: f a

data Parser a where
    PZero :: Parser a
    POneOf  :: (Eq a, Show a) => PElem a -> Parser a
    PAlt  :: [Parser a] -> Parser a
    POneOrMore :: Parser a -> Parser a
    PSeq  :: Monoid a => [Parser a] -> Parser a

deriving instance Show (Parser a)
deriving instance Eq (Parser a)
deriving instance Ord a => Ord (Parser a)

instance Opaque Parser where
    none     = PZero
    p1 ^| p2 = PAlt [p1, p2]

oneOrMore p         = POneOrMore p
zeroOrMore p         = none ^| oneOrMore p

instance Monoid a => Monoid (Parser a) where
    mempty        = PZero
    x `mappend` y = PSeq [y,x]

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

type RunParser a = (Parser a -> Stream a -> [(Stream a, [a])])

runParserSome :: forall a. Eq a => RunParser a -> Parser a -> Stream a -> [(Stream a, [a])]
runParserSome r p s = do
    (s', t) <- r p s
    res <- (s', []) : runParserSome r p s'
    return $ fmapParserResult (t++) res

runParserAlt :: Eq a => RunParser a ->  [Parser a] -> Stream a -> [(Stream a, [a])]
runParserAlt r ps s = concat $ map (flip r s) ps

runParserSeq :: forall a t. (Eq a, Monoid a) => RunParser a -> [Parser a] -> Stream a -> [(Stream a, [a])]
runParserSeq r ps stream = foldr flattenSeq [(stream, mempty)] ps
    where
        -- flattenSeq :: Parser a -> [(Stream a, a)] -> [(Stream a, a)]
        flattenSeq p alts = do
            (s, t) <- alts
            (s', t') <- r p s
            return (s', t <> t')

runParserOneOf :: Eq a => PElem a -> Stream a -> [(Stream a, [a])]
runParserOneOf p s =
    case runPElem p s of
    Nothing -> []
    Just (s', t) -> [(s', [t])]

runParser'' :: forall a. Eq a => RunParser a -> Parser a -> Stream a -> [(Stream a, [a])]
runParser'' r  PZero _ = []
runParser'' r (POneOf p) s = runParserOneOf p s
runParser'' r (POneOrMore p) s = runParserSome r p s
runParser'' r (PAlt ps) s = runParserAlt r ps s
runParser'' r (PSeq ps) s = runParserSeq r ps s

runParser' Nothing _ _ = []
runParser' (Just r) p s = runParser'' r p s

runParser :: Ord a => RunParser a
runParser = decycle2 runParser'

----------------------------------------------------------------------

is :: (Eq t, Show t) => [t] -> Parser t
is = POneOf . PElem

are :: (Eq a, Show a) => [a] -> Parser a
are = zeroOrMore . is

allChars = ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
isChar f = is $ filter f allChars
lower = isChar Char.isLower
digit = isChar Char.isDigit
upper = isChar Char.isUpper
letter = isChar Char.isLetter
alphaNum = isChar Char.isAlphaNum

str = oneOrMore letter

space = isChar Char.isSpace

openPar = is ['(']
closePar = is [')']

-- withParens :: Parser Char a -> Parser Char a
-- withParens x = openPar *> x <* closePar

-- optParens :: Parser Char a -> Parser Char a
-- optParens x = x ^| withParens x


