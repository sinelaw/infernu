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
               | LitFunc [String] expr
               | Call expr [expr]
               | Assign expr expr -- lvalue must be a property (could represent a variable)
               | Property expr String  -- lvalue must be a JObject
               | Index expr expr  -- lvalue must be a JArray
               | Var String
          deriving (Show, Eq)

data Expr a = Expr (Body (Expr a)) a
          deriving (Show, Eq)

-- there are no variables. a variable is a property accessor on either the global scope or an anonymous local object.

data TypeError = TypeError String
               deriving (Show, Eq)

data Context = Global | Context { parent :: Context, vars :: [(String, Type)], curType :: Type }
             deriving (Show, Eq)

mkContext :: Context -> Type -> Context
mkContext ctx t = Context ctx [] t

rightExpr :: Context -> Body (Expr (Either a Context)) -> Type -> Expr (Either a Context)
rightExpr ctx body t = Expr body (Right . Context ctx [] $ t)

exprData :: Expr t -> t
exprData (Expr _ t) = t

setType :: Type -> Expr (Either TypeError Context) -> Expr (Either TypeError Context)
setType t (Expr body (Right ctx)) = Expr body (Right $ Context ctx [] t)
setType _ _ = error "Expecting expression with no type errors" -- TODO

getType :: Expr (Either TypeError Context) -> Maybe Type
getType (Expr _ (Left _)) = Nothing
getType (Expr _ (Right t)) = Just . curType $ t

inferType :: Expr (Either TypeError Context) -> Expr (Either TypeError Context)
inferType e@(Expr _ (Left _)) = e
inferType (Expr body (Right ctx)) = 
    case curType ctx of
      Top -> inferred
      TVar name -> inferred -- TODO deduce that "name" must be the type given by inferred
      _ -> if (curType ctx) == (fromJust . getType $ inferred)
           then inferred
           else Expr body (Left $ TypeError "type mismatch")
    where inferred = 
              case body of 
                LitBoolean _ -> rightExpr ctx body JBoolean
                LitNumber _ -> rightExpr ctx body JNumber
                LitString _ -> rightExpr ctx body JString
                LitRegex _ -> rightExpr ctx body JRegex

                LitFunc argNames funcBody -> 
                    Expr newBody funcType
                    where argTypes = map (const $ TVar "todo") argNames
                          newFuncBody = inferType funcBody
                          funcType = either makeBadBody makeFuncType
                                     $ exprData newFuncBody
                          makeBadBody = const . Left $ TypeError "func body is badly typed"
                          makeFuncType bodyContext = Right . newContext $ JFunc argTypes (curType bodyContext)
                          newBody = LitFunc argNames newFuncBody
                          newContext t = Context ctx (zip argNames argTypes) t
                                            
                LitArray [] -> rightExpr ctx body (JArray $ TVar "todo") -- TODO: generate unique name
                LitArray (x:xs) -> 
                    if isRight headType
                    then if areSameType 
                         then rightExpr ctx newBody 
                                  $ JArray . curType . fromRight 
                                  $ headType
                         else makeErrorExpr "could not infer array type: elements are of inconsistent type"
                    else makeErrorExpr "array head is badly typed"
                    where headType = exprData . inferType $ x
                          restTypes = map inferType xs
                          areSameType = all ((headType ==) . exprData) restTypes
                          newBody = LitArray $ (inferType x) : restTypes
                          makeErrorExpr str = Expr newBody . Left $ TypeError str
                                                
                LitObject xs -> 
                    if any isLeft (map snd propTypes)
                    then Expr newBody . Left $ TypeError "could not infer object type: properties are badly typed"
                    else rightExpr ctx newBody 
                             . JObject 
                             . map (\(name, expr) -> (name, curType . fromRight $ expr)) 
                             $ propTypes
                    where propNamedTypes = map (\(name, expr) -> (name, inferType expr)) xs
                          propTypes = map (\(name, expr) -> (name, exprData expr)) propNamedTypes
                          newBody = LitObject propNamedTypes

                Property objExpr propName ->
                    case inferredObjExpr of
                      Expr _ (Right (Context _ _ (JObject props))) -> 
                          case lookup propName props of
                            Nothing -> makeError $ "object type has no property '" ++ propName ++ "'"
                            Just propType -> rightExpr ctx newBody propType
                      _ -> makeError "property accessor on non-object"
                    where makeError = Expr newBody . Left . TypeError
                          inferredObjExpr = inferType objExpr
                          newBody = Property inferredObjExpr propName

                Call callee args ->
                    case inferredCallee of
                      Expr _ (Right (Context _ _ (JFunc reqArgTypes returnType))) -> 
                          if any isLeft inferredArgTypes
                          then makeError "some arguments are badly typed"
                          else if any id 
                                   $ zipWith (/=) reqArgTypes 
                                   $ map (curType . fromRight) inferredArgTypes
                                      -- TODO: support polymorphism - handle type variables in the req list
                               then makeError "argument types do not match callee"
                               else Expr newBody (Right $ mkContext ctx returnType)
                      _ -> makeError "call target is not a callable type"
                    where makeError = Expr newBody . Left . TypeError
                          newBody = Call inferredCallee inferredArgs
                          inferredCallee = inferType callee
                          inferredArgs = map inferType args
                          inferredArgTypes = map exprData inferredArgs

                Var name -> 
                    case lookup name (vars ctx) of
                      Nothing -> Expr body (Right ctx)
                      Just t -> Expr body (Right $ Context ctx [] t)

                x -> Expr body $ Left $ TypeError ("expression not implemented: " ++ show x)


fromRight :: (Show a, Show b)=> Either a b -> b
fromRight (Right x) = x
fromRight e = error $ "Expected Right: " ++ (show e)

newExpr :: Body (Expr (Either a Context)) -> Expr (Either a Context)
newExpr x = Expr x $ Right $ Context Global [] Top

bla = inferType (newExpr (LitArray [newExpr $ LitString "3", newExpr $ LitNumber 3]))
blo = inferType (newExpr (LitObject [("test", newExpr $ LitString "3"), ("mest", newExpr $ LitNumber 3)]))
blf = inferType . newExpr . LitFunc ["x", "y"] . newExpr $ LitArray [newExpr $ Property (newExpr $ LitString "3") "x"]

-- function (x, y) { return [ { x: "bla" }.x ]; }
myFunc = newExpr . LitFunc ["x", "y"] . newExpr $ LitArray [newExpr $ Property (newExpr $ LitObject [("x", newExpr $ LitString "bla")]) "x"]

blf1 = inferType . newExpr $ Call myFunc [newExpr $ LitString "1", newExpr $ LitString "2"]

