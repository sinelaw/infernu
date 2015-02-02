module Inferno.BuiltinArray
       (arrayRowType)
       where

import Inferno.Types

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

array :: Type -> Type
array t = Fix $ TCons TArray [t]

boolean :: Fix FType
boolean = Fix $ TBody TBoolean

ts :: t -> TScheme t
ts = TScheme []

arrayProps :: Type -> [(String, TypeScheme)]
arrayProps elemType = let aType = array elemType in
  [ ("length", ts number)
  , ("concat", ts $ func aType aType aType)
     -- TODO support thisArg (requires type variables)
  , ("every", ts $ func aType (funcN [undef, elemType, number, aType, boolean]) boolean) -- missing thisArg
  , ("filter", ts $ func aType (funcN [undef, elemType, number, aType, boolean]) aType) -- missing thisArg
    -- TODO support optional argument for fromIndex (last parameter)
  , ("indexOf", ts $ funcN [aType, elemType, number, number])
  , ("join", ts $ func aType string string)
  , ("lastIndexOf", ts $ func aType number number)
--  , "map" -- requires type variables, and maybe foralls on row properties
  , ("pop", ts $ funcN [aType, elemType])
  , ("push", ts $ funcN [aType, elemType, number])
  , ("reverse", ts $ funcN [aType, aType])
  , ("shift", ts $ funcN [aType, elemType])
  , ("slice", ts $ funcN [aType, number, number, aType])
  , ("some", ts $ func aType (funcN [undef, elemType, number, aType, boolean]) aType) -- missing thisArg
  , ("sort", ts $ func aType (funcN [undef, elemType, elemType, number]) aType)
  , ("splice", ts $ funcN [aType, number, number, aType])
  , ("unshift", ts $ funcN [aType, elemType])
  ]

arrayRowType :: Type -> TRowList Type
arrayRowType elemType = foldr addProp (TRowEnd Nothing) $ arrayProps elemType
  where addProp (name, t) rowlist = TRowProp name t rowlist

