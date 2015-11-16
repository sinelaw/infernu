{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Util
       where

import           Control.Monad      (forM)

import           Infernu.InferState (fresh, Infer)
import           Infernu.Lib        (safeLookup)
import           Infernu.Prelude
import           Infernu.Expr       (EPropName(..))
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
array t = Fix $ TApp (TCons TArray (KArrow KStar KStar)) [t]

stringMap :: Type -> Type
stringMap t = Fix $ TApp (TCons TStringMap (KArrow KStar KStar)) [t]

boolean :: Fix FType
boolean = Fix $ TBody TBoolean

ref :: Type -> Type
ref t = Fix $ TApp (TCons TRef (KArrow KStar KStar)) [t]

undef :: Type
undef = Fix $ TBody TUndefined

ts :: [Int] -> t -> TScheme t
ts vs t = TScheme (map (flip Flex KStar) vs) $ qualEmpty t

tsq :: [Int] -> TQual t -> TScheme t
tsq vs = TScheme (map (flip Flex KStar) vs)

ty :: t -> TScheme t
ty t = TScheme [] $ qualEmpty t

tvar :: Int -> Type
tvar n = Fix . TBody . TVar $ Flex n KStar

tcons :: TConsName -> Kind -> [Type] -> FType Type
tcons n k ts' = TApp (TCons n k') ts'
    where
        k' = karrow k (map kind ts')

withTypeClass :: String -> a -> a -> TQual a
withTypeClass n t t' = TQual { qualPred = [TPredIsIn { predClass = ClassName n, predType = t }], qualType = t' }

openRow :: Int -> Type
openRow tv = record Nothing $ TRowEnd $ Just $ RowTVar (Flex tv KRow)

prop :: String -> TScheme t -> TRowList t -> TRowList t
prop name = TRowProp (TPropGetName $ EPropName name)

addProp :: VarNames t => TRowList t -> (String, TScheme t) -> Infer (TRowList t)
addProp rowlist (name, propTS) =
  do allocNames <- forM (schemeVars propTS) $ \tvName -> (tvName,) . (flip Flex $ kind tvName) <$> fresh
     let ts' = mapVarNames (safeLookup allocNames) propTS
     return $ TRowProp (TPropGetName $ EPropName name) ts' rowlist

