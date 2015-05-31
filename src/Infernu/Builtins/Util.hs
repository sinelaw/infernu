module Infernu.Builtins.Util
       where

import Infernu.Types
import           Infernu.Prelude

func :: Type -> Type -> Type -> Type
func this x y = Fix $ TFunc [this, x] y (TRowEnd Nothing)

funcN :: [Fix FType] -> Fix FType -> Fix FType
funcN xs tres = Fix $ TFunc xs tres (TRowEnd Nothing)

string :: Type
string = Fix $ TBody TString

regex :: Type
regex = Fix $ TBody TRegex

nullT :: Type
nullT = Fix $ TBody TNull

number :: Type
number = Fix $ TBody TNumber

array :: Type -> Type
array t = Fix $ TCons TArray [t]

boolean :: Fix FType
boolean = Fix $ TBody TBoolean

undef :: Type
undef = Fix $ TBody TUndefined

ts :: [Int] -> t -> TScheme t
ts vs t = TScheme (map Flex vs) $ qualEmpty t

ty :: t -> TScheme t
ty t = TScheme [] $ qualEmpty t

tvar :: Int -> Type
tvar = Fix . TBody . TVar . Flex

withTypeClass :: String -> a -> a -> TQual a
withTypeClass n t t' = TQual { qualPred = [TPredIsIn { predClass = ClassName n, predType = t }], qualType = t' }
