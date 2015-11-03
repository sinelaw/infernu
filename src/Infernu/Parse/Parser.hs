{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | A simple applicative parser

{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Parse.Parser where

import           Control.Applicative (Alternative(..), (<|>))
import           Data.List (intercalate)


import           Data.Monoid ((<>), mconcat)
import qualified Data.Char as Char

import           Infernu.Prelude

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
    empty          = PZero
    p1      <|> p2 = PAlt [p1, p2]

    some p = PSome p
    many p = some p <|> pure []

instance Monoid a => Monoid (Parser s a) where
    mempty        = PZero
    x `mappend` y = PSeq [y,x]

cons :: Parser s a -> Parser s [a] -> Parser s [a]
cons x xs = (fmap (:[]) x) <> xs

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

-- flatten :: [Maybe a] -> Maybe [a]
-- flatten = emptyToNothing . catMaybes
--     where
--         emptyToNothing [] = Nothing
--         emptyToNothing xs = Just xs

runParserSome :: forall a t. Parser a t -> Stream a -> [(Stream a, [t])]
runParserSome p s = do
    (s', t) <- runParser p s
    res <- (s', []) : runParserSome p s'
    return $ fmapParserResult (t:) res

runParserAlt :: [Parser a t] -> Stream a -> [(Stream a, t)]
runParserAlt ps s = concat $ map (flip runParser s) ps

runParserSeq :: forall a t. Monoid t => [Parser a t] -> Stream a -> [(Stream a, t)]
runParserSeq ps stream = foldr flattenSeq [(stream, mempty)] ps
    where
        flattenSeq :: Parser a t -> [(Stream a, t)] -> [(Stream a, t)]
        flattenSeq p alts = do
            (s, t) <- alts
            (s', t') <- runParser p s
            return (s', t <> t')

runParser :: forall a t. Parser a t -> Stream a -> [(Stream a, t)]
runParser PZero _ = []
runParser (POne p) s =
    case runParserSingle p s of
    Nothing -> []
    Just (s', t) -> [(s', t)]
runParser (PApp pf px) s = do
    (s1, f) <- runParser pf s
    (s2, x) <- runParser px s1
    return (s2, f x)
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


