-- |

{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Parse.Types where

--import           Control.Applicative  (many, some, (<^), (^>), (^||^), empty)

import           Infernu.Parse.NameRec as N
import           Infernu.Parse.Parser as P
import           Infernu.Prelude


type TParser = Parser Char

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

spaces = fmap (const Space) $ many space
inSpace p = spaces ^> p <^ spaces
arrow = fmap (const Arrow) $ is (== '-') ^> is (== '>')
fatArrow = fmap (const FatArrow) $ is (== '=') ^> is (== '>')
colon = fmap (const Colon) $ is (== ':')
comma = fmap (const Comma) $ is (== ',')
sstr = inSpace str

someInTuple x = optParens (fmap (:[]) x)
                ^||^ withParens (x `P.cons` some (comma ^> x))

identChar = P.alphaNum ^||^ P.oneOf "_'"

tvarName :: TParser String
tvarName = inSpace $ P.lower `P.cons` many identChar

constrName :: TParser String
constrName = inSpace $ P.upper `P.cons` many identChar

tvar = Var <$> tvarName
fun = Fun <$> (someInTuple body <^ inSpace arrow) <^^> body
app = App <$> constructor <^^> body
body = inSpace $ optParens $ inSpace (fun ^||^ tvar ^||^ app)

constructor = Constructor <$> constrName
constraint = Constraint <$> typeClass <^^> body
typeClass = TypeClass <$> constrName

constraints = (inSpace (someInTuple constraint) <^ inSpace fatArrow) ^||^ pure []
ptype = PType <$> constraints <^^> body


