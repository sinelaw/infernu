module Test where

-- TODO:
-- * Blocks, statements, etc.
-- * Write engine that steps through statements in a program using info to infer types between expressions (e.g. in assignemnts)

import Data.Maybe(fromJust)
import Data.Either(isLeft, lefts, isRight)


data Type = Top
          | TVar String 
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
               | LitFunc [String] expr
          deriving (Show, Eq)

data Expr a = Expr (Body (Expr a)) a
          deriving (Show, Eq)

-- there are no variables. a variable is a property accessor on either the global scope or an anonymous local object.

data TypeError = TypeError String
               deriving (Show, Eq)

rightExpr :: Body (Expr (Either a b)) -> b -> Expr (Either a b)
rightExpr body t = Expr body (Right t)

exprData :: Expr t -> t
exprData (Expr _ t) = t

setType :: Type -> Expr (Either TypeError Type) -> Expr (Either TypeError Type)
setType t (Expr body _) = Expr body (Right t)

getType :: Expr (Either TypeError Type) -> Maybe Type
getType (Expr _ (Left _)) = Nothing
getType (Expr _ (Right t)) = Just t

inferType :: Expr (Either TypeError Type) -> Expr (Either TypeError Type)
inferType e@(Expr _ (Left _)) = e
inferType (Expr body (Right t)) = case t of
                            Top -> inferred
                            TVar name -> inferred -- TODO deduce that "name" must be the type given by inferred
                            _ -> if t == (fromJust . getType $ inferred)
                                 then inferred
                                 else Expr body (Left $ TypeError "type mismatch")
    where inferred = case body of 
                       LitBoolean _ -> rightExpr body JBoolean
                       LitNumber _ -> rightExpr body JNumber
                       LitString _ -> rightExpr body JString
                       LitRegex _ -> rightExpr body JRegex
                       LitFunc argNames funcBody -> 
                           Expr newBody funcType
                           where argTypes = map (const $ TVar "todo") argNames
                                 newFuncBody = inferType funcBody
                                 funcType = either (const . Left $ TypeError "func body is badly typed") (\bodyType -> Right $ JFunc argTypes bodyType) 
                                         $ exprData newFuncBody
                                 newBody = LitFunc argNames newFuncBody
                                            
                       LitArray [] -> rightExpr body (JArray $ TVar "todo") -- TODO: generate unique name
                       LitArray (x:xs) -> 
                           if isRight headType
                           then if areSameType 
                                then rightExpr newBody (JArray . fromRight $ headType)
                                else makeErrorExpr "could not infer array type: elements are of inconsistent type"
                           else makeErrorExpr "array head is badly typed"
                           where headType = exprData . inferType $ x
                                 restTypes = map inferType xs
                                 areSameType = all ((headType ==) . exprData) restTypes
                                 newBody = LitArray $ (inferType x) : restTypes
                                 makeErrorExpr str = Expr newBody 
                                                     $ Left 
                                                     $ TypeError str

                       LitObject xs -> 
                           if any isLeft (map snd propTypes)
                           then Expr newBody
                                    $ Left 
                                    $ TypeError "could not infer object type: properties are badly typed"
                           else rightExpr newBody . JObject $ map (\(name, expr) -> (name, fromRight expr)) propTypes
                           where propNamedTypes = map (\(name, expr) -> (name, inferType expr)) xs
                                 propTypes = map (\(name, expr) -> (name, exprData expr)) propNamedTypes
                                 newBody = LitObject propNamedTypes

                       Property objExpr propName ->
                           case objExpr of
                             Expr _ (Right $ JObject props) -> 
                                 case lookup propName props of
                                   
                             _ ->  Expr body 
                                        $ Left
                                        $ TypeError "property accessor on non-object"
                                 
                       x -> Expr body $ Left $ TypeError ("expression not implemented: " ++ show x)


fromRight :: (Show a, Show b)=> Either a b -> b
fromRight (Right x) = x
fromRight e = error $ "Expected Right: " ++ (show e)

newExpr :: Body (Expr (Either a Type)) -> Expr (Either a Type)
newExpr x = Expr x $ Right Top

bla = inferType (newExpr (LitArray [newExpr $ LitString "3", newExpr $ LitNumber 3]))
blo = inferType (newExpr (LitObject [("test", newExpr $ LitString "3"), ("mest", newExpr $ LitNumber 3)]))
blf = inferType . newExpr . LitFunc ["x", "y"] . newExpr $ LitArray [newExpr $ Property (newExpr $ LitString "3") "x"]

