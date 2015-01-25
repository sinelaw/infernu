module SafeJS.BuiltinArray
       (arrayRowType)
       where

import SafeJS.Types

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

arrayProps :: Type -> [(String, Type)]
arrayProps elemType = let aType = array elemType in
  [ ("length", number)
  , ("concat", func aType aType aType)
     -- TODO support thisArg (requires type variables)
  , ("every", func aType (funcN [undef, elemType, number, aType, boolean]) boolean) -- missing thisArg
  , ("filter", func aType (funcN [undef, elemType, number, aType, boolean]) aType) -- missing thisArg
    -- TODO support optional argument for fromIndex (last parameter)
  , ("indexOf", funcN [aType, elemType, number, number])
  , ("join", func aType string string)
  , ("lastIndexOf", func aType number number)
--  , "map" -- requires type variables, and maybe foralls on row properties
  , ("pop", funcN [aType, elemType])
  , ("push", funcN [aType, elemType, number])
  , ("reverse", funcN [aType, aType])
  , ("shift", funcN [aType, elemType])
  , ("slice", funcN [aType, number, number, aType])
  , ("some", func aType (funcN [undef, elemType, number, aType, boolean]) aType) -- missing thisArg
  , ("sort", func aType (funcN [undef, elemType, elemType, number]) aType)
  , ("splice", funcN [aType, number, number, aType])
  , ("unshift", funcN [aType, elemType])
  ]

arrayRowType :: Type -> TRowList Type
arrayRowType elemType = foldr addProp (TRowEnd Nothing) $ arrayProps elemType
  where addProp (name, t) rowlist = TRowProp name t rowlist

