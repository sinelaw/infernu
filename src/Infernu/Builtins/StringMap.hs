module Infernu.Builtins.StringMap where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types

stringMapRowType :: Monad m => Type -> m (TRowList Type)
stringMapRowType elemType = return
                            . TRowProp TPropSetIndex (ty $ funcN [aType, string, elemType] undef)
                            . TRowProp TPropGetIndex (ty $ func aType string elemType)
                            $ TRowEnd Nothing
    where aType = Fix $ TCons TStringMap [elemType]
