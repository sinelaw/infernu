{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Builtins.StringMap where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types
import           Infernu.Expr       (EPropName(..))

stringMapRowType :: Monad m => Type -> m (TRowList Type)
stringMapRowType elemType = return
                            . TRowProp (TPropGetName EPropSetIndex) (ty $ funcN [aType, string, elemType] undef)
                            . TRowProp (TPropGetName EPropGetIndex) (ty $ func aType string elemType)
                            $ TRowEnd Nothing
    where aType = Fix $ TCons TStringMap [elemType]
