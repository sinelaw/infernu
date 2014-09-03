module Test where

-- TODO:
-- * Blocks, statements, etc.
-- * Write engine that steps through statements in a program using info to infer types between expressions (e.g. in assignemnts)

import Data.Either(isLeft, lefts, isRight)

data Type = TVar String 
          | JBoolean | JNumber | JString | JRegex 
          | JArray Type 
          | JObject [(String, Type)] 
          | JFunc [Type] Type
          deriving (Show, Eq)

data Op = Plus | Minus | GreaterThan | LessThan | Equals
          deriving (Show, Eq)

--data LValue = Var String | StrIndex Expr String | NumIndex Expr Int
data Body expr = LitBoolean Bool 
               | LitNumber Double 
               | LitString String 
               | LitRegex String 
               | LitArray [expr] 
               | LitObject [(String, expr)]
               | Assign expr expr -- lvalue must be a property (could represent a variable)
               | Property expr String  -- lvalue must be a JObject
               | Index expr expr  -- lvalue must be a JArray
          deriving (Show, Eq)

data Expr a = Expr (Body (Expr a)) a
          deriving (Show, Eq)

-- there are no variables. a variable is a property accessor on either the global scope or an anonymous local object.

data TypeError = TypeError String [TypeError]
               deriving (Show, Eq)

rightExpr :: Body (Expr (Either a b)) -> b -> Expr (Either a b)
rightExpr body t = Expr body (Right t)

exprType :: Expr t -> t
exprType (Expr _ t) = t

inferType :: Expr (Either TypeError Type) -> Expr (Either TypeError Type)
inferType (Expr body _) = 
    case body of 
      LitBoolean _ -> rightExpr body JBoolean
      LitNumber _ -> rightExpr body JNumber
      LitString _ -> rightExpr body JString
      LitRegex _ -> rightExpr body JRegex
                                        
      LitArray [] -> rightExpr body (JArray $ TVar "name") -- TODO: generate unique name
      LitArray (x:xs) -> if (isRight headType) && areSameType
                         then rightExpr body (JArray . fromRight $ headType)
                         else Expr body 
                                  $ Left 
                                  $ TypeError "could not infer array type: elements are of inconsistent type" []
          where headType = exprType . inferType $ x
                areSameType = all ((headType ==) . exprType) 
                              $ map inferType xs

      LitObject xs -> if any isLeft propTypes 
                      then Expr body 
                               $ Left 
                               $ TypeError "could not infer object type: properties are badly typed" (lefts propTypes) 
                      else rightExpr body . JObject $ map (\(name, expr) -> (name, fromRight expr)) propNamedTypes
          where propNamedTypes = map (\(name, expr) -> (name, exprType . inferType $ expr)) xs
                propTypes = map snd propNamedTypes

      _ -> Expr body $ Left $ TypeError "not implemented" []


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Expected Right" 
