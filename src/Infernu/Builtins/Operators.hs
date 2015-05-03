module Infernu.Builtins.Operators
       (builtins)
       where

import           Infernu.Types
import           Infernu.Prelude

import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)

ts :: [Int] -> Type -> TypeScheme
ts tvs t = TScheme (map Flex tvs) (qualEmpty t)
           
unaryFunc :: Type -> Type -> TypeScheme
unaryFunc t1 t2 = ts [0] $ Fix $ TFunc [tVar 0, t1] t2

binaryFunc  :: Type -> Type -> Type -> Type -> Fix FType
binaryFunc tThis t1 t2 t3 = Fix $ TFunc [tThis, t1, t2] t3

binarySimpleFunc :: Type -> Type -> Type
binarySimpleFunc tThis t = Fix $ TFunc [tThis, t, t] t

binaryFuncS :: Type -> Type -> Type -> TypeScheme
binaryFuncS t1 t2 t3 = ts [0] $ binaryFunc (tVar 0) t1 t2 t3

tVar :: Int -> Type
tVar = Fix . TBody . TVar . Flex

tBoolean :: Type
tBoolean = Fix $ TBody TBoolean

tNumber :: Type
tNumber = Fix $ TBody TNumber

tString :: Type
tString = Fix $ TBody TString

numRelation :: TypeScheme
numRelation = binaryFuncS tNumber tNumber tBoolean

numOp :: TypeScheme
numOp = binaryFuncS tNumber tNumber tNumber

builtins :: Map EVarName TypeScheme
builtins = Map.fromList [
  ("!",            unaryFunc tBoolean tBoolean),
  ("~",            unaryFunc tNumber  tNumber),
  ("typeof",       ts [0, 1] $ Fix $ TFunc [tVar 1, tVar 0] tString),
  ("+",            TScheme [Flex 0, Flex 1] $ TQual { qualPred = [TPredIsIn (ClassName "Plus") (tVar 1)]
                                         , qualType = binarySimpleFunc (tVar 0) (tVar 1) }),
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
  ("===",          ts [0, 1, 2] $ Fix $ TFunc [tVar 2, tVar 0, tVar 1] tBoolean),
  ("!==",          ts [0, 1, 2] $ Fix $ TFunc [tVar 2, tVar 0, tVar 1] tBoolean),
  ("&&",           ts [0, 1] $ Fix $ TFunc [tVar 0, tVar 1, tVar 1] (tVar 1)),
  ("||",           ts [0, 1] $ Fix $ TFunc [tVar 0, tVar 1, tVar 1] (tVar 1)),
  -- avoid coercions on == and !=
  ("==",           ts [0, 1] $ Fix $ TFunc [tVar 1, tVar 0, tVar 0] tBoolean),
  ("!=",           ts [0, 1] $ Fix $ TFunc [tVar 1, tVar 0, tVar 0] tBoolean),
  ("RegExp",       ts [0] $ Fix $ TFunc [tVar 0, tString, tString] (Fix $ TBody TRegex)),
  ("String",       ts [1] $ Fix $ TFunc [Fix $ TBody $ TUndefined, tVar 1] (Fix $ TBody TString)),
  ("Number",       ts [1] $ Fix $ TFunc [Fix $ TBody $ TUndefined, tVar 1] (Fix $ TBody TNumber)),
  ("Boolean",      ts [1] $ Fix $ TFunc [Fix $ TBody $ TUndefined, tVar 1] (Fix $ TBody TBoolean)),
  ("NaN",          ts [] tNumber),
  ("Infinity",     ts [] tNumber),
  ("undefined",    ts [] $ Fix $ TBody TUndefined)
  ]
