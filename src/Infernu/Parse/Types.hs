-- |

{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Parse.Types where

import           Control.Applicative  (many, some, (<|>))


import           Infernu.Parse.Parser as P
import           Infernu.Prelude


type TParser s = Parser s Char

-- typeSig = :: type
-- type = constraint => body
-- constraint = (TypeClass body, ...)
-- body = a | (body,..) -> body | TypeConstructor [body...]

data Token = Space | Arrow | FatArrow | Colon | Comma
           deriving (Show)

data Body = Var String | Fun [Body] Body | App Constructor Body
          deriving Show
data Constructor = Constructor String
                 deriving Show
data Constraint = Constraint TypeClass Body
                deriving Show
data TypeClass = TypeClass String
               deriving Show
data PType = PType [Constraint] Body
             deriving Show

inSparens :: Stream s => Parser s Char a -> Parser s Char a
inSparens x = x <|> withParens (inSpace x)

spaces :: Stream s => Parser s Char Token
spaces = (fmap (const Space) $ many space) <?> "spaces"

inSpace :: Stream s => Parser s Char a -> Parser s Char a
inSpace p = (spaces *> p) <* spaces

arrow :: Stream s => Parser s Char Token
arrow = fmap (const Arrow) $ isStr "->" <?> "'->'"

fatArrow :: Stream s => Parser s Char Token
fatArrow = fmap (const FatArrow) (isStr "=>") <?> "'=>'"

colon :: Stream s => Parser s Char Token
colon = fmap (const Colon) (isStr "::") <?> "'::'"

comma :: Stream s => Parser s Char Token
comma = fmap (const Comma) (isChar ',') <?> "','"

someInTuple :: Stream s => Parser s Char a -> Parser s Char [a]
someInTuple x = fmap (:[]) x
                <|> withParens (x `P.cons` some (comma *> x))

identChar :: Stream s => Parser s Char Char
identChar = P.alphaNum <|> P.oneOf "_'"

tvarName :: Stream s => TParser s String
tvarName = P.lower `P.cons` many identChar

constrName :: Stream s => TParser s String
constrName = P.upper `P.cons` many identChar

tvar :: Stream s => Parser s Char Body
tvar = Var <$> tvarName <?> "tvar"

fun :: Stream s => Parser s Char Body -> Parser s Char Body
fun p = Fun <$> (someInTuple p <* inSpace arrow) <*> p

app :: Stream s => Parser s Char Body -> Parser s Char Body
app p = App <$> (constructor <* some space) <*> p

body' :: Stream s => Parser s Char Body -> Parser s Char Body
body' p = (inSparens (tvar <|> fun p <|> app p))

body :: Stream s => Parser s Char Body
body = pfix body' <?> "type-body"

constructor :: Stream s => Parser s Char Constructor
constructor = (Constructor <$> constrName) <?> "type-constructor"

constraint :: Stream s => Parser s Char Constraint
constraint = Constraint <$> typeClass <*> body

typeClass :: Stream s => Parser s Char TypeClass
typeClass = (TypeClass <$> constrName) <?> "typeclass-constriant"

constraints :: Stream s => Parser s Char [Constraint]
constraints = ((someInTuple constraint) <* inSpace fatArrow) <|> pure []

ptype :: Stream s => Parser s Char PType
ptype = PType <$> constraints <*> body


