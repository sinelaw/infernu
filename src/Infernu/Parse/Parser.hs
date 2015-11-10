{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A simple applicative parser
module Infernu.Parse.Parser
       ( Stream(..)
       , ListStream(..), lsNew
       , Parser, runParser, runParserFull
       , pfix, (<?>), isStr
       , isChar, cons, oneOf, followedBy, isSingle, is, are, lower, upper, letter, alphaNum, str, space
       , openPar, closePar, withParens, optParens
       ) where

import           Control.Applicative (Alternative(..), (<|>))
import           Data.List (intercalate, sortBy, nubBy)

import           Data.Monoid ((<>))
import qualified Data.Char as Char

import           Infernu.Prelude

class Stream s where
    streamPos     :: s a -> Int
    streamRead    :: s a -> Maybe (a, s a)
    streamIsEmpty :: s a -> Bool

streamPosEqual :: (Stream s1, Stream s2) => s1 a -> s2 b -> Bool
streamPosEqual s1 s2 = streamPos s1 == streamPos s2

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
    PFix :: Parser s a t -> Parser s a t
    PFirst :: Parser s a t -> Parser s a t' -> Parser s a t
    PLast :: Parser s a t' -> Parser s a t -> Parser s a t
    PName :: Parser s a t -> String -> Parser s a t

pfix :: (Parser s a t -> Parser s a t) -> Parser s a t
pfix f = f (pfix $ f . PFix)

infixl <?>
(<?>) :: Parser s a t -> String -> Parser s a t
(<?>) = PName

instance Show (Parser s a t) where
    show (PZero)       = "PZero"
    show (POne _)      = "POne"
    show (PAlt ps)     = "(" ++ intercalate " | " (map show ps) ++ ")"
    show (PApp pf px)  = "(" ++ show pf ++ " <*> " ++ show px ++ ")"
    show (PSome p)     = "some " ++ show p
    show (PAppend ps)  = "(" ++ intercalate ", " (map show ps) ++ ")"
    show (PFix p)      = "PFix" -- ++ show p
    show (PFirst p ps) = "(" ++ show p ++ " <* " ++ show ps ++ ")"
    show (PLast ps p)  = "(" ++ show ps ++ " *> " ++ show p ++ ")"
    show (PName _ s)   = s

instance Stream s => Functor (Parser s a) where
    fmap f p = pure f <*> p

instance Stream s => Applicative (Parser s a) where
    pure    = POne . pure
    p <*> x = PApp p x
    p1 <* p2 = PFirst p1 p2
    p1 *> p2 = PLast p1 p2

instance Stream s => Alternative (Parser s a) where
    empty          = PZero
    (PAlt p1) <|> (PAlt p2) = PAlt (p1 ++ p2)
    (PAlt p1) <|> p2        = PAlt (p1 ++ [p2])
    p1        <|> (PAlt p2) = PAlt (p1 : p2)
    p1        <|> p2        = PAlt [p1, p2]

    some p         = PSome p
    many p         = some p <|> pure []

instance (Stream s, Monoid t) => Monoid (Parser s a t) where
    mempty        = PZero
    (PAppend x) `mappend` (PAppend y) = PAppend (y ++ x)
    (PAppend x) `mappend` y           = PAppend (y : x)
    x           `mappend` (PAppend y) = PAppend (y ++ [x])
    x           `mappend` y           = PAppend [y,x]

cons :: Stream s => Parser s c a -> Parser s c [a] -> Parser s c [a]
cons x xs = (fmap (:[]) x) <> xs

fmapParserResult :: (a -> b) -> (s, a) -> (s, b)
fmapParserResult = fmap

type RunParser s c t = Parser s c t -> s c -> [(s c, t)]

runParserSome :: forall s c t. Stream s => RunParser s c t -> Parser s c t -> s c -> [(s c, [t])]
runParserSome r p s = do
    (s', t) <- r p s
    res <- (s', []) : runParserSome r p s'
    return $ fmapParserResult (t:) res

-- sortByGreediest :: Stream s => [(s c, t)] -> [(s c, t)]
-- sortByGreediest = sortBy (\(s1,_) (s2,_) -> streamPos s2 `compare` streamPos s1)

-- headOrEmpty :: [a] -> [a]
-- headOrEmpty [] = []
-- headOrEmpty (x:_) = [x]

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

nubByPos :: Stream s => [(s c, t)] -> [(s c, t)]
nubByPos = nubBy (\r1 r2 -> (fst r1) `streamPosEqual` (fst r2))

runParser' :: forall s c t. Stream s => s c -> RunParser s c t
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
runParser' sc (PAlt ps)     s = runParserAlt (runParser' sc) ps s
runParser' sc (PAppend ps)  s = runParserSeq (runParser' sc) ps s
runParser' sc (PFix _)      _ | streamIsEmpty sc = []
runParser' sc (PFix p)      s =
    case streamRead sc of
    Nothing -> []
    Just (_, sc') -> runParser' sc' p s
runParser' sc (PFirst p1 p2) s = do
    (s1, x) <- runParser' sc p1 s
    (s2, _) <- runParser' sc p2 s1
    return (s2, x)
runParser' sc (PLast p1 p2) s = do
    (s1, _) <- runParser' sc p1 s
    (s2, x) <- runParser' sc p2 s1
    return (s2, x)
runParser' sc (PName p _) s = runParser' sc p s

runParser :: Stream s => Parser s c t -> s c -> [(s c, t)]
runParser p s = runParser' s p s

runParserFull :: Stream s => Parser s c t -> s c -> Maybe (s c, t)
runParserFull p s = case empty of
    [] -> Nothing
    (x:_) -> Just x
    where
        (nonEmpty, empty) = break (\(s,_) -> streamIsEmpty s)
                            $ runParser p s
----------------------------------------------------------------------

isStr :: Stream s => String -> Parser s Char String
isStr str' = (POne $ ParserSingle (go str' [])) <?> str'
    where
        go [] rem' s = Just (s, rem')
        go (x:xs) rem' s = case streamRead s of
            Just (x', s') | x' == x -> go xs (x:rem') s'
            _ -> Nothing

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
lower = is Char.isLower <?> "lower"
upper :: Stream s => Parser s Char Char
upper = is Char.isUpper <?> "upper"
letter :: Stream s => Parser s Char Char
letter = (is Char.isLetter) <?> "letter"
alphaNum :: Stream s => Parser s Char Char
alphaNum = (is Char.isAlphaNum) <?> "alphanum"

isChar :: Stream s => Char -> Parser s Char Char
isChar c = is (==c) <?> (['\'', c, '\''])

followedBy :: Applicative f => f a -> f b -> f (a, b)
x `followedBy` y = (,) <$> x <*> y

str :: Stream s => Parser s Char String
str = some letter


space :: Stream s => Parser s Char Char
space = is Char.isSpace

openPar :: Stream s => Parser s Char Char
openPar = isChar '('

closePar :: Stream s => Parser s Char Char
closePar = isChar ')'

withParens :: Stream s => Parser s Char a -> Parser s Char a
withParens x = openPar *> x <* closePar

optParens :: Stream s => Parser s Char a -> Parser s Char a
optParens x = x <|> withParens x

-- Example

-- data Expr = Var Char | App Expr Expr
--           deriving Show

-- evar :: Stream s => Parser s Char Expr
-- evar = Var <$> letter

-- eapp :: Stream s => Parser s Char Expr -> Parser s Char Expr
-- eapp p = App <$> p <*> p

-- eexpr' :: Stream s => Parser s Char Expr -> Parser s Char Expr
-- eexpr' p = evar <|> eapp p

-- eexpr :: Stream s => Parser s Char Expr
-- eexpr = pfix eexpr'

