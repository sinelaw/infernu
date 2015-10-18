module Infernu.Builtins.Operators
       (builtins)
       where

import           Infernu.Prelude
import           Infernu.Types

import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map

import           Infernu.Builtins.Math (math)
import           Infernu.Builtins.Object (object)
import           Infernu.Builtins.Util
import qualified Infernu.Builtins.Names as Names
import           Infernu.Expr       (EPropName(..), EVarName)

unaryFunc :: Type -> Type -> TypeScheme
unaryFunc t1 t2 = ts [0] $ Fix $ TFunc [tvar 0, t1] t2

binaryFunc  :: Type -> Type -> Type -> Type -> Fix FType
binaryFunc tThis t1 t2 t3 = Fix $ TFunc [tThis, t1, t2] t3

binarySimpleFunc :: Type -> Type -> Type
binarySimpleFunc tThis t = Fix $ TFunc [tThis, t, t] t

binaryFuncS :: Type -> Type -> Type -> TypeScheme
binaryFuncS t1 t2 t3 = ts [0] $ binaryFunc (tvar 0) t1 t2 t3

numRelation :: TypeScheme
numRelation = binaryFuncS number number boolean

numOp :: TypeScheme
numOp = binaryFuncS number number number

builtins :: Map EVarName TypeScheme
builtins = Map.fromList [

    -- TODO: refOp and derefOp should be data constructors of the built-in Ref type (need to add
    -- support for data constructors)
    (Names.refOp,          ts [1] $ Fix $ TFunc [tvar 1] (tcons TRef [tvar 1])),
    (Names.refAssignOp,    ts [1] $ Fix $ TFunc [tcons TRef [tvar 1], tvar 1] (tvar 1)),
    (Names.derefOp,        ts [1] $ Fix $ TFunc [tcons TRef [tvar 1]] (tvar 1)),

    ("!",            unaryFunc boolean boolean),
    ("~",            unaryFunc number  number),
    ("typeof",       ts [0, 1] $ Fix $ TFunc [tvar 1, tvar 0] string),
    ("instanceof",   ts [0, 1, 2] $ Fix $ TFunc [tvar 2, tvar 0, tvar 1] boolean),
    ("+",            TScheme [(Flex 0 KStar), (Flex 1 KStar)]
                     TQual { qualPred = [TPredIsIn (ClassName "Plus") (tvar 1)]
                           , qualType = binarySimpleFunc (tvar 0) (tvar 1) }),
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
    ("===",          ts [0, 1, 2] $ Fix $ TFunc [tvar 2, tvar 0, tvar 1] boolean),
    ("!==",          ts [0, 1, 2] $ Fix $ TFunc [tvar 2, tvar 0, tvar 1] boolean),
    ("&&",           ts [0, 1] $ Fix $ TFunc [tvar 0, tvar 1, tvar 1] (tvar 1)),
    ("||",           ts [0, 1] $ Fix $ TFunc [tvar 0, tvar 1, tvar 1] (tvar 1)),
    -- avoid coercions on == and !=
    ("==",           ts [0, 1] $ Fix $ TFunc [tvar 1, tvar 0, tvar 0] boolean),
    ("!=",           ts [0, 1] $ Fix $ TFunc [tvar 1, tvar 0, tvar 0] boolean),
    ("RegExp",       ts [] $ Fix $ TFunc [undef, string, string] regex),
    ("String",       ts [1] $ Fix $ TFunc [undef, tvar 1] string),
    ("Number",       ts [1] $ Fix $ TFunc [undef, tvar 1] number),
    ("Boolean",      ts [1] $ Fix $ TFunc [undef, tvar 1] boolean),
    ("NaN",          ts [] number),
    ("Infinity",     ts [] number),
    ("undefined",    ts [0] undef),
    ("isFinite",     ts [0] $ Fix $ TFunc [tvar 0, number] boolean),
    ("isNaN",        ts [0] $ Fix $ TFunc [tvar 0, number] boolean),
    ("parseFloat",   ts [0] $ Fix $ TFunc [tvar 0, string] number),
    ("parseInt",     ts [0] $ Fix $ TFunc [tvar 0, string, number] number),
    ("decodeURI",    ts [0] $ Fix $ TFunc [tvar 0, string] string),
    ("decodeURIComponent",    ts [0] $ Fix $ TFunc [tvar 0, string] string),
    ("encodeURI",    ts [0] $ Fix $ TFunc [tvar 0, string] string),
    ("encodeURIComponent",    ts [0] $ Fix $ TFunc [tvar 0, string] string),
    ("Date",         ts [] $ record (Just "Date[Constructor]")
                                 $ TRowProp (TPropGetName EPropFun) (ts [] $ Fix $ TFunc [Fix $ TBody TDate] string)
                                 $ prop "now"   (ts [0] $ Fix $ TFunc [tvar 0] number)
                                 $ prop "parse" (ts [0] $ Fix $ TFunc [tvar 0, string] (Fix $ TBody TDate))
                                 $ prop "UTC"   (ts [0] $ Fix $ TFunc [tvar 0, number, number, number, number, number, number, number] (Fix $ TBody TDate))
                                 $ TRowEnd Nothing),
    ("Math",         math),
    ("Object",       object),
    ("JSON",         ts [] $ record (Just "JSON")
                                 $ prop "stringify" (ts [0, 1] $ Fix $ TFunc [tvar 0, tvar 1] string)
                                 -- TODO: should really be "maybe (tvar 1)"
                                 $ prop "parse"     (ts [0, 1] $ Fix $ TFunc [tvar 0, string] (tvar 1))
                                 $ TRowEnd Nothing)
    ]
