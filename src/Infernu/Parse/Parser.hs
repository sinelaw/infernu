{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | A simple applicative parser

{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Parse.Parser where

import Control.Applicative (Alternative(..), (<|>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), mconcat)
import qualified Data.Char as Char

import Infernu.Prelude

type Stream a = [a]

emptyStream :: [a]
emptyStream = []

data ParserSingle a t = ParserSingle (Stream a -> Maybe (Stream a, t))

unParserSingle :: ParserSingle s a -> Stream s -> Maybe (Stream s, a)
unParserSingle (ParserSingle p) = p

runParserSingle :: ParserSingle s a -> Stream s -> Maybe (Stream s, a)
runParserSingle (ParserSingle p) s = p s

instance Functor (ParserSingle s) where
    fmap f (ParserSingle p) = ParserSingle $ \s ->
        {-# SCC "ParserSingle_fmap" #-}
        case p s of
        Nothing -> Nothing
        Just (s', t) -> Just (s', f t)


instance Applicative (ParserSingle s) where
    pure x = ParserSingle $ \s -> Just (s, x)
    (ParserSingle pf) <*> ppx =
        {-# SCC "ParserSingle_<*>" #-}
        ParserSingle $ \s -> case pf s of
        Nothing -> Nothing
        Just (s', f) -> case unParserSingle ppx s' of
            Nothing -> Nothing
            Just (s'', x) -> Just (s'', f x)

instance Alternative (ParserSingle s) where
    empty = ParserSingle $ const Nothing
    (ParserSingle px) <|> y =
        {-# SCC "ParserSingle_<|>" #-}
        ParserSingle $ \s ->
        case px s of
        Nothing -> unParserSingle y s
        Just (s', t) -> Just (s', t)


data Parser a t where
    PZero :: Parser a t
    POne :: (ParserSingle a t) -> Parser a t
    PAlt :: [Parser a t] -> Parser a t
    PApp :: (Parser a (u -> t)) -> (Parser a u) -> Parser a t
    PSome :: Parser a t -> Parser a [t]
    PSeq :: Monoid t => [Parser a t] -> Parser a t

instance Show (Parser a t) where
    show (PZero) = "PZero"
    show (POne _) = "POne"
    show (PAlt ps) = "(" ++ intercalate " | " (map show ps) ++ ")"
    show (PApp pf px) = show pf ++ " <*> " ++ show px
    show (PSome p) = "some " ++ show p
    show (PSeq ps) = "(" ++ intercalate ", " (map show ps) ++ ")"

instance Functor (Parser s) where
    fmap f p = pure f <*> p

instance Applicative (Parser s) where
    pure = POne . pure
    p <*> x = PApp p x

instance Alternative (Parser s) where
    empty                         = PZero
    p       <|> PZero   = p
    PZero   <|> p       = p
    PAlt xs <|> PAlt ys = PAlt $ xs ++ ys
    PAlt xs <|> p       = PAlt $ xs ++ [p]
    p       <|> PAlt xs = PAlt $ p : xs
    p1      <|> p2      = PAlt [p1, p2]

    some p = PSome p
    many p = some p <|> pure []

instance Monoid a => Monoid (Parser s a) where
    mempty = PZero
    PZero `mappend` p = p
    p `mappend` PZero = p
    (PSeq xs) `mappend` (PSeq ys) = PSeq (ys ++ xs)
    (PSeq xs) `mappend` y = PSeq (y : xs)
    x `mappend` (PSeq ys) = PSeq (ys ++ [x])
    x `mappend` y = PSeq [y,x]

cons :: Parser s a -> Parser s [a] -> Parser s [a]
cons x xs = (fmap (:[]) x) <> xs

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

flatten :: [Maybe [a]] -> Maybe [a]
flatten = fmap concat . sequence

runParserSome :: forall a t. Parser a t -> Stream a -> Maybe [(Stream a, [t])]
runParserSome p s =
    case runParser p s of
    Nothing -> Nothing
    Just alts -> flatten $ map go alts
    where
        prependX :: t -> [(Stream a, [t])] -> [(Stream a, [t])]
        prependX x res = map (fmapParserResult (x:)) res
        go :: (Stream a, t) -> Maybe [(Stream a, [t])]
        go (s1, x1) =
            case runParser p s1 of
            Nothing -> Just [(s1, [x1])]
            Just alts' -> fmap (prependX x1) . flatten $ Just [(s1, [])] : map go alts'

runParserAlt :: [Parser a t] -> Stream a -> Maybe [(Stream a, t)]
runParserAlt ps s = flatten $ map (flip runParser s) ps

runParserSeq :: forall a t. Monoid t => [Parser a t] -> Stream a -> Maybe [(Stream a, t)]
runParserSeq ps s = foldr flattenSeq (Just [(s, mempty)]) ps
    where
        flattenSeq :: Parser a t -> Maybe [(Stream a, t)] -> Maybe [(Stream a, t)]
        flattenSeq p Nothing = Nothing
        flattenSeq p (Just alts) = flattenSeqAlt p alts
        flattenSeqAlt :: Parser a t -> [(Stream a, t)] -> Maybe [(Stream a, t)]
        flattenSeqAlt p alts = flatten nextAltses
            where
                combineAlts :: t -> [(Stream a, t)] -> [(Stream a, t)]
                combineAlts t [] = error "TODO should avoid this statically"
                combineAlts t ((s2, t2) : alts) = (s2, t <> t2) : combineAlts t alts
                nextAltses :: [Maybe [(Stream a, t)]]
                nextAltses = map (\(s, t) -> fmap (combineAlts t) $ runParser p s) alts

runParser :: forall a t. Parser a t -> Stream a -> Maybe [(Stream a, t)]
runParser PZero _ = Nothing
runParser (POne p) s =
    case runParserSingle p s of
    Nothing -> Nothing
    Just (s', t) -> Just [(s', t)]
runParser (PApp pf px) s =
    case runParser pf s of
    Nothing -> Nothing
    Just altFs -> fmap concat . sequence $ map appAlt altFs
        where appAlt (s', f) =
                  case runParser px s' of
                  Nothing -> Nothing
                  Just altXs -> Just $ map (\(s'', x) -> (s'', f x)) altXs
runParser (PSome p) s = runParserSome p s
runParser (PAlt ps) s = runParserAlt ps s
runParser (PSeq ps) s = runParserSeq ps s

----------------------------------------------------------------------

isSingle :: (t -> Bool) -> ParserSingle t t
isSingle f = ParserSingle $
           \s -> case s of
           (x:s') | f x -> Just (s', x)
           _ -> Nothing

is :: (t -> Bool) -> Parser t t
is = POne . isSingle

are :: (a -> Bool) -> Parser a [a]
are = many . is

oneOf opts = is (\x -> elem x opts)

lower = is Char.isLower
upper = is Char.isUpper
letter = is Char.isLetter
alphaNum = is Char.isAlphaNum

x `followedBy` y = (,) <$> x <*> y


str = some letter

space = is Char.isSpace

openPar = is (=='(')
closePar = is (==')')

withParens :: Parser Char a -> Parser Char a
withParens x = openPar *> x <* closePar

optParens :: Parser Char a -> Parser Char a
optParens x = x <|> withParens x


