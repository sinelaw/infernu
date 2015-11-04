{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

data Parser s a p where
    PZero      :: Parser s a p
    POneOf     :: PElem s a -> Parser s a p
    PAlt       :: [p]   -> Parser s a p
    POneOrMore :: p     -> Parser s a p
    PSeq       :: [p]   -> Parser s a p

data Named n f = Named n (f (Named n f))

class Opaque n f where
    infixl ^|
    (^|) :: n f -> n f -> f (n f)
    none :: n f

named n p = (Named n p)

deriving instance (Show p, Show s, Show a) => Show (Parser s a p)
-- deriving instance (Eq s, Eq a)     => Eq (Parser s a p)
-- deriving instance (Ord s, Ord a)   => Ord (Parser s a p)

instance Opaque (Named Int) (Parser s a) where
    none     = named 0 PZero
    p1 ^| p2 = PAlt [p1, p2]

oneOrMore p  = POneOrMore p
zeroOrMore p = none ^| oneOrMore p

instance Monoid (Parser s a p) where
    mempty        = PSeq []
    x `mappend` y = PSeq [y,x]

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

type RunParser s a p = (Parser s a p -> Stream s -> [(Stream s, [a])])

runParserSome :: RunParser s a p -> Parser s a p -> Stream s -> [(Stream s, [a])]
runParserSome r p s = do
    (s', t) <- r p s
    res <- (s', []) : runParserSome r p s'
    return $ fmapParserResult (t++) res

runParserAlt :: RunParser s a p ->  [Parser s a p] -> Stream s -> [(Stream s, [a])]
runParserAlt r ps s = concat $ map (flip r s) ps

runParserSeq :: RunParser s a p -> [Parser s a p] -> Stream s -> [(Stream s, [a])]
runParserSeq r ps stream = foldr flattenSeq [(stream, mempty)] ps
    where
        -- flattenSeq :: Parser s a p -> [(Stream s, a)] -> [(Stream s, a)]
        flattenSeq p alts = do
            (s, t) <- alts
            (s', t') <- r p s
            return (s', t <> t')

runParserOneOf :: Ord s => PElem s a -> Stream s -> [(Stream s, [a])]
runParserOneOf p s =
    case runPElem p s of
    Nothing -> []
    Just (s', t) -> [(s', [t])]

runParser'' :: Ord s => RunParser s a p -> Parser s a p -> Stream s -> [(Stream s, [a])]
runParser'' r  PZero _ = []
runParser'' r (POneOf p) s = runParserOneOf p s
runParser'' r (POneOrMore p) s = runParserSome r p s
runParser'' r (PAlt ps) s = runParserAlt r ps s
runParser'' r (PSeq ps) s = runParserSeq r ps s

runParser' Nothing _ _ = []
runParser' (Just r) p s = runParser'' r p s

runParser :: (Ord s, Ord a) => RunParser s a p
runParser = decycle2 runParser'

----------------------------------------------------------------------

is :: Ord s => Map s a -> Parser s a p
is = POneOf . PElem

are :: Ord s => Map s a -> Parser s a p
are = zeroOrMore . is

allChars = ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
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


