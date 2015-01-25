module SafeJS.BuiltinArray
       (arrayRowType)
       where

import SafeJS.Types

func :: Type -> Type -> Type -> Type
func this x y = Fix $ TCons TFunc [this, x, y]

string :: Type
string = Fix $ TBody TString

number :: Type
number = Fix $ TBody TNumber

undef :: Type
undef = Fix $ TBody TUndefined

array :: Type -> Type
array t = Fix $ TCons TArray [t]

var :: TVarName -> Type
var name = Fix $ TBody (TVar name)

arrayProps :: Type -> [(String, Type)]
arrayProps elemType = let aType = array elemType in
  [ ("length", number)
  , ("concat", func aType aType aType)
     -- TODO support thisArg (requires type variables)
  , ("every", func aType (func undef number aType) aType)
  , ("filter", func aType (func undef number aType) aType)
    -- TODO support optional argument for fromIndex (last parameter)
  , ("indexOf", Fix $ TCons TFunc [aType, elemType, number, number])
  , ("join", func aType string string)
  , ("lastIndexOf", func aType number number)
--  , "map" -- requires type variables, and maybe foralls on row properties
  , ("pop", Fix $ TCons TFunc [aType, elemType])
  , ("push", func aType elemType aType)
  ]

arrayRowType :: Type -> TRowList Type
arrayRowType elemType = foldr addProp (TRowEnd Nothing) $ arrayProps elemType
  where addProp (name, t) rowlist = TRowProp name t rowlist

