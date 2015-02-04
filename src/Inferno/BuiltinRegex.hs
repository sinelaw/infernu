{-# LANGUAGE TupleSections #-}
module Inferno.BuiltinRegex
       (regexRowType)
       where

import           Control.Monad             (foldM, forM)
import Inferno.Types
import Inferno.InferState
import           Inferno.Lib (safeLookup)

func :: Type -> Type -> Type -> Type
func this x y = Fix $ TCons TFunc [this, x, y]

funcN :: [Fix FType] -> Fix FType
funcN xs = Fix $ TCons TFunc xs

string :: Type
string = Fix $ TBody TString

number :: Type
number = Fix $ TBody TNumber

undef :: Type
undef = Fix $ TBody TUndefined

regex :: Type
regex = Fix $ TBody TRegex

boolean :: Fix FType
boolean = Fix $ TBody TBoolean

ts :: t -> TScheme t
ts = TScheme []

tvar :: TVarName -> Type
tvar = Fix . TBody . TVar

regexProps :: [(String, TypeScheme)]
regexProps = let aType = regex in
  [ ("source", ts string)
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
regexRowType :: Infer (TRowList Type)
regexRowType = foldM addProp (TRowEnd Nothing) $ regexProps
  where addProp rowlist (name, propTS) =
          do allocNames <- forM (schemeVars propTS) $ \tvName -> (fresh >>= return . (tvName,))
             let ts' = mapVarNames (safeLookup allocNames) propTS
             return $ TRowProp name ts' rowlist
