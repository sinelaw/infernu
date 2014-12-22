module SafeJS.Builtins
       (builtins,
        opLogicalNot, opBinaryNot, opPrefixPlus, opPrefixMinus, opPrefixTypeof)
       where

import           SafeJS.Types
import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)

unaryFunc :: Type -> Type -> TScheme
unaryFunc t1 t2 = TScheme [] $ Fix $ TCons TFunc [t1, t2]

tBoolean :: Type
tBoolean = Fix $ TBody TBoolean

tNumber :: Type
tNumber = Fix $ TBody TNumber

tString :: Type
tString = Fix $ TBody TString

opLogicalNot :: EVarName
opLogicalNot = "!"

opBinaryNot :: EVarName
opBinaryNot = "~"

opPrefixPlus :: EVarName
opPrefixPlus = "+"

opPrefixMinus :: EVarName
opPrefixMinus = "-"

opPrefixTypeof :: EVarName
opPrefixTypeof = "typeof"

builtins :: Map EVarName TScheme
builtins = Map.fromList [
  (opLogicalNot,   unaryFunc tBoolean tBoolean),
  (opBinaryNot,    unaryFunc tNumber  tNumber),
  (opPrefixPlus,   unaryFunc tNumber  tNumber),
  (opPrefixMinus,  unaryFunc tNumber  tNumber),
  (opPrefixTypeof, TScheme [0] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 0, tString])
  ]
