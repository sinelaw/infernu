module Inferno.Builtins
       (builtins)
       where

import           Inferno.Types
import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)

unaryFunc :: Type -> Type -> TScheme
unaryFunc t1 t2 = TScheme [0] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 0, t1, t2]

binaryFunc :: Type -> Type -> Type -> TScheme
binaryFunc t1 t2 t3 = TScheme [0] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 0, t1, t2, t3]

tBoolean :: Type
tBoolean = Fix $ TBody TBoolean

tNumber :: Type
tNumber = Fix $ TBody TNumber

tString :: Type
tString = Fix $ TBody TString

numRelation :: TScheme
numRelation = binaryFunc tNumber tNumber tBoolean

numOp :: TScheme
numOp = binaryFunc tNumber tNumber tNumber

boolRelation :: TScheme
boolRelation = binaryFunc tBoolean tBoolean tBoolean

builtins :: Map EVarName TScheme
builtins = Map.fromList [
  ("!",            unaryFunc tBoolean tBoolean),
  ("~",            unaryFunc tNumber  tNumber),
  ("typeof",       TScheme [0,1] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 1, Fix $ TBody $ TVar 0, tString]),
  ("+",            numOp),
  ("-",            numOp),
  ("*",            numOp),
  ("/",            numOp),
  ("%",            numOp),
  ("<<",           numOp),
  (">>",           numOp),
  (">>>",          numOp),
  ("&",            numOp),
  ("^",            numOp),
  ("|",            numOp),
  ("<",            numRelation),
  ("<=",           numRelation),
  (">",            numRelation),
  (">=",           numRelation),
  ("===",          TScheme [0, 1, 2] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 2, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1, tBoolean]),
  ("!==",          TScheme [0, 1, 2] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 2, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1, tBoolean]),
  ("&&",           boolRelation),
  ("||",           boolRelation),
  -- avoid coercions on == and !=
  ("==",           TScheme [0, 1] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 1, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 0, tBoolean]),
  ("!=",           TScheme [0, 1] $ Fix $ TCons TFunc [Fix $ TBody $ TVar 1, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 0, tBoolean])
  ]
