{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | A simple applicative parser

module Infernu.Parse.Parser where

import Control.Applicative (Alternative(..), (<|>))
import Data.List (intercalate)
import Data.Monoid ((<>))
import qualified Data.Char as Char

import Infernu.Prelude

type Stream a = [a]

emptyStream :: [a]
emptyStream = []

data ParserSingle a t = ParserSingle (Stream a -> Maybe (Stream a, t))

unParserSingle (ParserSingle p) = p

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
    show (PSeq ps) = "(" ++ intercalate " " (map show ps) ++ ")"

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
    (PSeq xs) `mappend` (PSeq ys) = PSeq (xs ++ ys)
    (PSeq xs) `mappend` y = PSeq (xs ++ [y])
    x `mappend` (PSeq ys) = PSeq (x : ys)
    x `mappend` y = PSeq [x,y]

cons :: (Functor f, Monoid (f [a])) => f a -> f [a] -> f [a]
cons x xs = fmap (:[]) x <> xs

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

runParser :: Parser a t -> Stream a -> Maybe (Stream a, [t])
runParser PZero _ = Nothing
runParser (POne p) s =
    case runParserSingle p s of
    Nothing -> Nothing
    Just (s', t) -> Just (s', [t])
runParser (PApp pf px) s =
    case runParser pf s of
    Nothing -> Nothing
    Just (s', fs) -> case runParser px s' of
        Nothing -> Nothing
        Just (s'', xs) -> Just (s'', concatMap (\x -> map ($ x) fs) xs)
runParser (PSome p) s =
    case runParser p s of
    Nothing -> Nothing
    Just (s', x) -> fmap (fmapParserResult reverse) $ go s' [x]
    where
        go s1 x1 =
            case runParser p s1 of
            Nothing -> Just (s1, x1)
            Just (s2, x2) -> go s2 (x2 : x1)

runParser (PAlt ps) s = firstJust $ map (flip runParser s) ps
    where firstJust []              = Nothing
          firstJust (Just x  : mxs) = Just x
          firstJust (Nothing : mxs) = firstJust mxs

runParser (PSeq ps) s = foldr accumParse (Just (s, mempty)) ps
    where
        accumParse p' Nothing = Nothing
        accumParse p' (Just (s', x')) =
            case runParser p' s' of
            Nothing -> Nothing
            Just (s'', x'') -> Just (s'', x'' <> x')

----------------------------------------------------------------------

isSingle f = ParserSingle $
           \s -> case s of
           (x:s') | f x -> Just (s', x)
           _ -> Nothing

is = POne . isSingle

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


