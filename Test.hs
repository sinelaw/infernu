module Test where

import Data.Either(isLeft, lefts)

data Type = Unknown | JBoolean | JNumber | JString | JRegex | JArray Type | JObject [(String, Type)] | JFunc [Type] Type
          deriving (Show, Eq)

data Op = Plus | Minus | GreaterThan | LessThan | Equals
          deriving (Show, Eq)

--data LValue = Var String | StrIndex Expr String | NumIndex Expr Int
data Expr = LitBoolean Bool | LitNumber Double | LitString String | LitRegex String | LitArray [Expr] | LitObject [(String, Expr)]
          | BinOp Op Expr Expr 
          | Assign Expr Expr
          | Property Expr String  -- lvalue must be a JObject
          | Index Expr Expr  -- lvalue must be a JArray
          | GlobalObject
          deriving (Show, Eq)

-- there are no variables. a variable is a property accessor on either the global scope or an anonymous local object.

data TypeError = TypeError String Expr [TypeError]
               deriving (Show, Eq)

inferType :: Expr -> Either TypeError Type
inferType (LitBoolean _) = Right JBoolean
inferType (LitNumber _) = Right JNumber
inferType (LitString _) = Right JString
inferType (LitRegex _) = Right JRegex
inferType (LitArray []) = Right $ JArray Unknown
inferType ar@(LitArray (x:xs))
    | areSameType = fmap JArray headType
    | otherwise = Left $ TypeError "array elements are of inconsistent type" ar []
    where headType = inferType x
          areSameType = all (headType ==) $ map inferType xs

inferType obj@(LitObject xs) 
    | any isLeft propTypes = Left $ TypeError "properties are badly typed" obj (lefts propTypes) 
    | otherwise = Right . JObject $ map (\(name, expr) -> (name, fromRight expr)) propNamedTypes
    where propNamedTypes = map (\(name, expr) -> (name, inferType expr)) xs
          propTypes = map snd propNamedTypes

inferType expr@(BinOp op a b)
    | (aType == bType) = if (any isLeft types)
                         then Left $ TypeError "error in expression" expr (lefts types)
                         else case op of 
                           Plus -> case (fromRight aType) of
                                     JString -> Right JString
                                     JNumber -> Right JNumber
                                     _ -> Left $ TypeError "operator requires numbers or strings only" expr []
                           Minus -> requireNumber aType aType
                           GreaterThan -> requireNumber aType (Right JBoolean)
                           LessThan -> requireNumber aType (Right JBoolean)
                           Equals -> Right JBoolean
    | otherwise = Left $ TypeError "refusing to coerce mismatching types in binary expression" expr []
    where aType = inferType a
          bType = inferType b
          types = [aType, bType]
          requireNumber t tRes = if (fromRight t) == JNumber 
                                 then tRes 
                                 else Left $ TypeError "operator requires numbers only" expr [] 

inferType expr = Left $ TypeError "not implemented" expr []


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Expected Right" 
