module Infernu.Builtins.Math
       (math)
       where

import           Infernu.Builtins.Util
import           Infernu.Prelude
import           Infernu.Types
import           Infernu.Expr       (EPropName(..))

numProp :: String -> TRowList Type -> TRowList Type
numProp name = TRowProp (TPropGetName $ EPropName name) $ ts [] number

numFuncProp :: String -> TRowList (Fix FType) -> TRowList (Fix FType)
numFuncProp name = TRowProp (TPropGetName $ EPropName name) $ ts [0] $ Fix $ TFunc [tvar 0, number] number

numFuncProp2 :: String -> TRowList (Fix FType) -> TRowList (Fix FType)
numFuncProp2 name = TRowProp (TPropGetName $ EPropName name) $ ts [0] $ Fix $ TFunc [tvar 0, number, number] number

math :: TScheme (Fix FType)
math = ts [] $ Fix $ TRow (Just "Math")
                   $ numProp "E"
                   $ numProp "LN10"
                   $ numProp "LN2"
                   $ numProp "LOG10E"
                   $ numProp "LOG2E"
                   $ numProp "PI"
                   $ numProp "SQRT1_2"
                   $ numProp "SQRT2"
                   $ numFuncProp "abs"
                   $ numFuncProp "acos"
--                             $ numFuncProp "acosh"
                   $ numFuncProp "asin"
--                           $ numFuncProp "asinh"
                   $ numFuncProp "atan"
                   $ numFuncProp2 "atan2"
--                               $ numFuncProp "atanh"
--                               $ numFuncProp "cbrt"
                   $ numFuncProp "ceil"
--                               $ numFuncProp "clz32"
                   $ numFuncProp "cos"
--                               $ numFuncProp "cosh"
                   $ numFuncProp "exp"
--                               $ numFuncProp "expm1"
                   $ numFuncProp "floor"
                   --   $ numFuncProp "fround"
                   --   $ numFuncProp "hypot"
                   --   $ numFuncProp "imul"
                   $ numFuncProp "log"
                   --   $ numFuncProp "log10"
                   --   $ numFuncProp "log1p"
                   --   $ numFuncProp "log2"
                   -- Requires support for variable number of arguments
                   $ numFuncProp2 "max"
                   $ numFuncProp2 "min"
                   $ numFuncProp2 "pow"
                   $ TRowProp (TPropGetName $ EPropName "random") (ts [0] $ Fix $ TFunc [tvar 0] number)
                   $ numFuncProp "round"
                   $ numFuncProp "sign"
                   $ numFuncProp "sin"
                   --   $ numFuncProp "sinh"
                   $ numFuncProp "sqrt"
                   $ numFuncProp "tan"
                   --   $ numFuncProp "tanh"
                   --   $ numFuncProp "trunc"
                   $ TRowEnd Nothing
