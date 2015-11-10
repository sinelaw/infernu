{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A simple applicative parser
module Infernu.Parse.Parser
       ( Stream(..)
       , ListStream(..), lsNew
       , Parser, runParser
       , cons, oneOf, followedBy, isSingle, is, are, lower, upper, letter, alphaNum, str, space
       , openPar, closePar, withParens, optParens
       ) where

import           Control.Applicative (Alternative(..), (<|>))
import           Data.List (intercalate, sortBy)


import           Data.Monoid ((<>))
import qualified Data.Char as Char

import           Infernu.Prelude

class Stream s where
    streamPos     :: s a -> Int
    streamRead    :: s a -> Maybe (a, s a)
    streamIsEmpty :: s a -> Bool

data ListStream a = ListStream { lsData :: [a], lsPos :: Int }
                  deriving Show

lsNew :: [a] -> ListStream a
lsNew xs = ListStream xs 0

instance Stream ListStream where
    streamPos                          = lsPos

    streamIsEmpty (ListStream [] _)    = True
    streamIsEmpty (ListStream (_:_) _) = False

    streamRead (ListStream [] _)       = Nothing
    streamRead (ListStream (x:xs) p)   = Just (x, ListStream xs (p+1))

data ParserSingle s c t = Stream s => ParserSingle (s c -> Maybe (s c, t))

unParserSingle :: Stream s => ParserSingle s c a -> s c -> Maybe (s c, a)
unParserSingle (ParserSingle p) = p

runParserSingle :: Stream s => ParserSingle s c a -> s c -> Maybe (s c, a)
runParserSingle (ParserSingle p) s = p s

instance Stream s => Functor (ParserSingle s c) where
    fmap f (ParserSingle p) = ParserSingle $ \s ->
        {-# SCC "ParserSingle_fmap" #-}
        case p s of
        Nothing -> Nothing
        Just (s', t) -> Just (s', f t)


instance Stream s => Applicative (ParserSingle s c) where
    pure x = ParserSingle $ \s -> Just (s, x)
    (ParserSingle pf) <*> ppx =
        {-# SCC "ParserSingle_<*>" #-}
        ParserSingle $ \s -> case pf s of
        Nothing -> Nothing
        Just (s', f) -> case unParserSingle ppx s' of
            Nothing -> Nothing
            Just (s'', x) -> Just (s'', f x)

instance Stream s => Alternative (ParserSingle s c) where
    empty = ParserSingle $ const Nothing
    (ParserSingle px) <|> y =
        {-# SCC "ParserSingle_<|>" #-}
        ParserSingle $ \s ->
        case px s of
        Nothing -> unParserSingle y s
        Just (s', t) -> Just (s', t)


data Parser s a t where
    PZero :: Parser s a t
    POne :: (ParserSingle s a t) -> Parser s a t
    PAlt :: [Parser s a t] -> Parser s a t
    PApp :: (Parser s a (u -> t)) -> (Parser s a u) -> Parser s a t
    PSome :: Parser s a t -> Parser s a [t]
    PAppend :: Monoid t => [Parser s a t] -> Parser s a t

instance Show (Parser s a t) where
    show (PZero)      = "PZero"
    show (POne _)     = "POne"
    show (PAlt ps)    = "(" ++ intercalate " | " (map show ps) ++ ")"
    show (PApp pf px) = show pf ++ " <*> " ++ show px
    show (PSome p)    = "some " ++ show p
    show (PAppend ps)    = "(" ++ intercalate ", " (map show ps) ++ ")"

instance Stream s => Functor (Parser s a) where
    fmap f p = pure f <*> p

instance Stream s => Applicative (Parser s a) where
    pure    = POne . pure
    p <*> x = PApp p x

instance Stream s => Alternative (Parser s a) where
    empty          = PZero
    p1      <|> p2 = PAlt [p1, p2]

    some p         = PSome p
    many p         = some p <|> pure []

instance (Stream s, Monoid t) => Monoid (Parser s a t) where
    mempty        = PZero
    x `mappend` y = PAppend [y,x]

cons :: Stream s => Parser s c a -> Parser s c [a] -> Parser s c [a]
cons x xs = (fmap (:[]) x) <> xs

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

type RunParser s a t = Parser s a t -> s a -> [(s a, t)]

runParserSome :: forall s c t. Stream s => RunParser s c t -> Parser s c t -> s c -> [(s c, [t])]
runParserSome r p s = do
    (s', t) <- r p s
    res <- (s', []) : runParserSome r p s'
    return $ fmapParserResult (t:) res

runParserAlt :: Stream s => RunParser s c t -> [Parser s c t] -> s c -> [(s c, t)]
runParserAlt r ps s = concat $ map (flip r s) ps

runParserSeq :: forall s c t. (Stream s, Monoid t) => RunParser s c t -> [Parser s c t] -> s c -> [(s c, t)]
runParserSeq r ps stream = foldr flattenSeq [(stream, mempty)] ps
    where
        flattenSeq :: Parser s c t -> [(s c, t)] -> [(s c, t)]
        flattenSeq p alts = do
            (s, t) <- alts
            (s', t') <- r p s
            return (s', t <> t')

runParser' :: forall s c t. Stream s => s c -> RunParser s c t
runParser' _  _ s | streamIsEmpty s  = []
runParser' _ PZero _ = []
runParser' _ (POne p) s =
    case runParserSingle p s of
    Nothing -> []
    Just (s', t) -> [(s', t)]
runParser' sc (PApp pf px) s = do
    (s1, f) <- runParser' sc pf s
    (s2, x) <- runParser' sc px s1
    return (s2, f x)
runParser' sc (PSome p)     s = runParserSome (runParser' sc) p s
runParser' sc (PAlt _)      _ | streamIsEmpty sc = []
runParser' sc (PAlt ps)     s =
    case streamRead sc of
    Nothing -> []
    Just (_, sc') -> runParserAlt (runParser' sc') ps s
runParser' sc (PAppend ps)  s = runParserSeq (runParser' sc) ps s

runParser :: Stream s => Parser s c t -> s c -> [(s c, t)]
runParser p s = sortBy (\(s1,_) (s2,_) -> streamPos s2 `compare` streamPos s1) $ runParser' s p s

----------------------------------------------------------------------

isSingle :: Stream s => (t -> Bool) -> ParserSingle s t t
isSingle f = ParserSingle $
           \s -> case streamRead s of
           Just (x, s') | f x -> Just (s', x)
           _ -> Nothing

is :: Stream s => (t -> Bool) -> Parser s t t
is = POne . isSingle

are :: Stream s => (a -> Bool) -> Parser s a [a]
are = many . is

oneOf :: (Eq a, Foldable t, Stream s) => t a -> Parser s a a
oneOf opts = is (\x -> elem x opts)

lower :: Stream s => Parser s Char Char
lower = is Char.isLower
upper :: Stream s => Parser s Char Char
upper = is Char.isUpper
letter :: Stream s => Parser s Char Char
letter = is Char.isLetter
alphaNum :: Stream s => Parser s Char Char
alphaNum = is Char.isAlphaNum

followedBy :: Applicative f => f a -> f b -> f (a, b)
x `followedBy` y = (,) <$> x <*> y

str :: Stream s => Parser s Char String
str = some letter

space :: Stream s => Parser s Char Char
space = is Char.isSpace

openPar :: Stream s => Parser s Char Char
openPar = is (=='(')

closePar :: Stream s => Parser s Char Char
closePar = is (==')')

withParens :: Stream s => Parser s Char a -> Parser s Char a
withParens x = openPar *> x <* closePar

optParens :: Stream s => Parser s Char a -> Parser s Char a
optParens x = x <|> withParens x

-- Example

-- data Expr = Var Char | App Expr Expr
--           deriving Show

-- evar :: Stream s => Parser s Char Expr
-- evar = Var <$> letter

-- eapp :: Stream s => Parser s Char Expr
-- eapp = App <$> eexpr <*> eexpr

-- eexpr :: Stream s => Parser s Char Expr
-- eexpr = evar <|> eapp
