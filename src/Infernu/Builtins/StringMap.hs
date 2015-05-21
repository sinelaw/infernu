module Infernu.Builtins.StringMap where

import Infernu.Types
import           Infernu.Prelude
import Infernu.Builtins.Util

stringMapRowType :: Monad m => Type -> m (TRowList Type)
stringMapRowType elemType = return
                            . TRowProp TPropSetIndex (ty $ funcN [aType, string, elemType] undef)
                            . TRowProp TPropGetIndex (ty $ func aType string $ maybeT elemType)
                            $ TRowEnd Nothing
    where aType = Fix $ TCons TStringMap [elemType]
