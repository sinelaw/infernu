module Infernu.Builtins.Object
       (object)
       where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types

keyObj :: Int -> Type -> TQual Type
keyObj tv = withTypeClass "StringKeys" (tvar tv)

object :: TScheme (Fix FType)
object = ts []
         $ record (Just "Object")
         -- assign - ES6
         $ prop "create"              (ts [0, 1] $ funcN [tvar 0, openRow 1] (openRow 1))
         -- can't be supported in a sane way:
         -- "defineProperties"
         -- "defineProperty"
         -- "getOwnPropertyDescriptor"
         $ prop "freeze"              (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] (tvar 1))
         $ prop "getOwnPropertyNames" (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] (array string))
         -- "getPrototypeOf" -- should we just return the same type? we don't support prototypes
         $ prop "isExtensible"        (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] boolean)
         $ prop "isFrozen"            (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] boolean)
         $ prop "isSealed"            (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] boolean)
         $ prop "keys"                (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] (array string))
         $ prop "preventExtensions"   (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] (tvar 1))
         $ prop "seal"                (tsq [0, 1] $ keyObj 1 $ funcN [tvar 0, tvar 1] (tvar 1))
         $ TRowEnd Nothing
