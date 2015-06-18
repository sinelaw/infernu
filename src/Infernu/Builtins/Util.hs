{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Util
       where

import           Control.Monad      (forM)

import           Infernu.InferState (fresh, Infer)
import           Infernu.Lib        (safeLookup)
import           Infernu.Prelude
import           Infernu.Types

func :: Type -> Type -> Type -> Type
func this x y = Fix $ TFunc [this, x] y

funcN :: [Fix FType] -> Fix FType -> Fix FType
funcN xs tres = Fix $ TFunc xs tres

string :: Type
string = Fix $ TBody TString

date :: Type
date = Fix $ TBody TDate

regex :: Type
regex = Fix $ TBody TRegex

nullT :: Type
nullT = Fix $ TBody TNull

number :: Type
number = Fix $ TBody TNumber

array :: Type -> Type
array t = Fix $ TCons TArray [t]

stringMap :: Type -> Type
stringMap t = Fix $ TCons TStringMap [t]

boolean :: Fix FType
boolean = Fix $ TBody TBoolean

undef :: Type
undef = Fix $ TBody TUndefined

ts :: [Int] -> t -> TScheme t
ts vs t = TScheme (map Flex vs) $ qualEmpty t

tsq :: [Int] -> TQual t -> TScheme t
tsq vs = TScheme (map Flex vs)

ty :: t -> TScheme t
ty t = TScheme [] $ qualEmpty t

tvar :: Int -> Type
tvar = Fix . TBody . TVar . Flex

withTypeClass :: String -> a -> a -> TQual a
withTypeClass n t t' = TQual { qualPred = [TPredIsIn { predClass = ClassName n, predType = t }], qualType = t' }

openRow :: Int -> Type
openRow tv = Fix $ TRow Nothing $ TRowEnd $ Just $ RowTVar (Flex tv)

prop :: EPropName -> TScheme t -> TRowList t -> TRowList t
prop name = TRowProp (TPropName name)

addProp :: VarNames t => TRowList t -> (EPropName, TScheme t) -> Infer (TRowList t)
addProp rowlist (name, propTS) =
  do allocNames <- forM (schemeVars propTS) $ \tvName -> (tvName,) . Flex <$> fresh
     let ts' = mapVarNames (safeLookup allocNames) propTS
     return $ TRowProp (TPropName name) ts' rowlist
