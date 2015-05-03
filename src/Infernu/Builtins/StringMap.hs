module Infernu.Builtins.StringMap where

import Infernu.Types

undef :: Type
undef = Fix $ TBody TUndefined

func :: Type -> Type -> Type -> Type
func this x y = Fix $ TFunc [this, x] y

funcN :: [Fix FType] -> Fix FType -> Fix FType
funcN xs tres = Fix $ TFunc xs tres

string :: Type
string = Fix $ TBody TString

ts :: t -> TScheme t
ts t = TScheme [] $ qualEmpty t

stringMapRowType elemType = return
                            . TRowProp TPropSetIndex (ts $ funcN [aType, string, elemType] undef)
                            . TRowProp TPropGetIndex (ts $ func aType string elemType)
                            $ TRowEnd Nothing
    where aType = Fix $ TCons TStringMap [elemType]
