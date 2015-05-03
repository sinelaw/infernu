module Infernu.Builtins
       (builtins)
       where

import           Infernu.Types
import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)

ts :: [TVarName] -> Type -> TypeScheme
ts tvs t = TScheme tvs (qualEmpty t)

emptyProto :: TRowList t
emptyProto = TRowEnd Nothing
                  
unaryFunc :: Type -> Type -> TypeScheme
unaryFunc t1 t2 = ts [0, 1] $ Fix $ TFunc [Fix $ TBody $ TVar 0, t1] t2 emptyProto

binaryFunc  :: Type -> Type -> Type -> Type -> Fix FType
binaryFunc tThis t1 t2 t3 = Fix $ TFunc [tThis, t1, t2] t3 emptyProto

binarySimpleFunc :: Type -> Type -> Type
binarySimpleFunc tThis t = Fix $ TFunc [tThis, t, t] t emptyProto

binaryFuncS :: Type -> Type -> Type -> TypeScheme
binaryFuncS t1 t2 t3 = ts [0] $ binaryFunc (Fix $ TBody $ TVar 0) t1 t2 t3

tVar :: TVarName -> Type
tVar = Fix . TBody . TVar

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
  ("typeof",       ts [0,1] $ Fix $ TFunc [Fix $ TBody $ TVar 1, Fix $ TBody $ TVar 0] tString emptyProto),
  ("+",            TScheme [0,1] $ TQual { qualPred = [TPredIsIn (ClassName "Plus") (tVar 1)]
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
  ("===",          ts [0, 1, 2] $ Fix $ TFunc [Fix $ TBody $ TVar 2, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1] tBoolean emptyProto),
  ("!==",          ts [0, 1, 2] $ Fix $ TFunc [Fix $ TBody $ TVar 2, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 1] tBoolean emptyProto),
  ("&&",           ts [0, 1] $ Fix $ TFunc [tVar 0, tVar 1, tVar 1] (tVar 1) emptyProto),
  ("||",           ts [0, 1] $ Fix $ TFunc [tVar 0, tVar 1, tVar 1] (tVar 1) emptyProto),
  -- avoid coercions on == and !=
  ("==",           ts [0, 1] $ Fix $ TFunc [Fix $ TBody $ TVar 1, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 0] tBoolean emptyProto),
  ("!=",           ts [0, 1] $ Fix $ TFunc [Fix $ TBody $ TVar 1, Fix $ TBody $ TVar 0, Fix $ TBody $ TVar 0] tBoolean emptyProto),
  ("RegExp",       ts [0] $ Fix $ TFunc [Fix $ TBody $ TVar 0, tString, tString] (Fix $ TBody TRegex) emptyProto),
  ("String",       ts [1] $ Fix $ TFunc [Fix $ TBody $ TUndefined, Fix $ TBody $ TVar 1] (Fix $ TBody TString) emptyProto),
  ("Number",       ts [1] $ Fix $ TFunc [Fix $ TBody $ TUndefined, Fix $ TBody $ TVar 1] (Fix $ TBody TNumber) emptyProto),
  ("Boolean",      ts [1] $ Fix $ TFunc [Fix $ TBody $ TUndefined, Fix $ TBody $ TVar 1] (Fix $ TBody TBoolean) emptyProto),
  ("NaN",          ts [] tNumber)
  ]
