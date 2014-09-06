{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Test where

-- TODO:
-- * Blocks, statements, etc.
-- * Write engine that steps through statements in a program using info to infer types between expressions (e.g. in assignemnts)

--import Data.List(intersperse)
import Data.Maybe(fromJust)--, isJust)
--import Data.Either(isLeft, lefts, isRight)
import Text.PrettyPrint.GenericPretty(Generic(..), Out(..), pp)
import Data.Traversable(Traversable(..))
import Data.Foldable(Foldable(..))

import Control.Monad.State
 
import Prelude hiding (foldr)

data Type = Top
          | TVar Int
          | JBoolean | JNumber | JString | JRegex 
          | JArray Type 
          | JObject [(String, Type)] 
          | JFunc [Type] Type
          deriving (Show, Eq, Generic)

instance Out Type

--data LValue = Var String | StrIndex Expr String | NumIndex Expr Int
data Body expr = LitBoolean Bool 
               | LitNumber Double 
               | LitString String 
               | LitRegex String 
               | Var String
               | LitFunc [String] [String] [expr]
               | LitArray [expr] 
               | LitObject [(String, expr)]
               | Call expr [expr]
               | Assign expr expr -- lvalue must be a property (could represent a variable)
               | Property expr String  -- lvalue must be a JObject
               | Index expr expr  -- lvalue must be a JArray
               | Return expr
          deriving (Show, Eq, Generic, Functor, Foldable, Traversable)



instance (Out a) => Out (Body a)

data Expr a = Expr (Body (Expr a)) a
          deriving (Show, Eq, Generic)


instance (Out a) => Out (Expr a)

commafy :: [String] -> String
commafy [] = []
commafy (x:[]) = x
commafy (x:xs) = x ++ ", " ++ (commafy xs)

toJs :: Expr a -> String
toJs (Expr body _) = 
    case body of
      LitBoolean x -> if x then "true" else "false"
      LitNumber x -> show x
      LitString s -> "'" ++ s ++ "'" -- todo escape
      LitRegex regex -> "/" ++ regex ++ "/" -- todo correctly
      LitArray xs -> "[ " ++ (commafy $ map toJs xs) ++ " ]"
      LitObject xs -> "{ " ++ (commafy $ map (\(name, val) -> name ++ ": " ++ (toJs val)) xs) ++ " }"

      LitFunc args varNames exprs -> "function (" ++ argsJs ++ ") " ++ block
          where argsJs = commafy $ args
                block =  "{\n" ++ vars' ++ "\n" ++ statements ++ " }\n"
                statements = (concat $ map (++ ";\n") $ map toJs exprs)
                vars' = "var " ++ commafy varNames ++ ";"

      Call callee args -> (toJs callee) ++ "(" ++ (commafy $ map toJs args) ++ ")"
      Assign target src -> (toJs target) ++ " = " ++ (toJs src)
      Property obj name -> (toJs obj) ++ "." ++ name
      Index arr idx -> (toJs arr) ++ "[" ++ (toJs idx) ++ "]"
      Var name -> name
      Return expr -> "return " ++ toJs expr



data TypeError = TypeError String
               deriving (Show, Eq, Generic)

instance Out TypeError



data VarScope = Global | VarScope { parent :: VarScope, vars :: [(String, Type)] }
               deriving (Show, Eq, Generic)

instance Out VarScope

data TypeScope = TypeScope { tVars :: [(Int, Maybe Type)] }
               deriving (Show, Eq, Generic)

instance Out TypeScope

data FuncScope = FuncScope { funcVars :: [(String, Type)]
                           , returnType :: Type }
               deriving (Show, Eq, Generic)

instance Out FuncScope


data Scope = Scope { typeScope :: TypeScope
                   , funcScope :: Maybe FuncScope }
               deriving (Show, Eq, Generic)

instance Out Scope

getVarType :: VarScope -> String -> Maybe Type
getVarType Global _ = Nothing
getVarType scope name = case lookup name (vars scope) of
                       Nothing -> getVarType (parent scope) name
                       Just t -> Just t

intrVars :: [String] -> VarScope -> State Scope VarScope
intrVars names scope = do
  vs <- forM names $ \name -> do
          varType' <- allocTVar
          return (name, varType')

  return $ VarScope { parent = scope, vars = vs }

allocTVar' :: TypeScope -> (Type, TypeScope)
allocTVar' tscope = (TVar allocedNum, updatedScope)
    where updatedScope = TypeScope { tVars = (allocedNum, Nothing) : oldTVars }
          oldTVars = tVars tscope
          maxNum = foldr max 0 $ map fst oldTVars
          allocedNum = maxNum + 1


allocTVar :: State Scope Type
allocTVar = do
  scope <- get
  let typeScope' = typeScope scope
      (varType', typeScope'') = allocTVar' typeScope'
  put $ scope { typeScope = typeScope'' }
  return varType'


emptyTypeScope :: TypeScope
emptyTypeScope = TypeScope []

emptyScope :: Scope
emptyScope = Scope { typeScope = emptyTypeScope, funcScope = Nothing }

-- rightExpr :: Scope -> Body (Expr (Scope, (Either a b))) -> b -> Expr (Scope, (Either a b))
-- rightExpr scope body x = Expr body (scope, Right x)

exprData :: Expr t -> t
exprData (Expr _ t) = t


pushTVar :: Int -> Type -> Type -> Type
pushTVar _ _ Top = Top
pushTVar x t (TVar y)
          | x == y = t
          | otherwise = TVar y
pushTVar _ _ JBoolean = JBoolean
pushTVar _ _ JNumber = JNumber
pushTVar _ _ JString = JString
pushTVar _ _ JRegex = JRegex
pushTVar x t (JArray u) = JArray $ pushTVar x t u
pushTVar x t (JObject props) = JObject $ map (\(propName, propT) -> (propName, pushTVar x t propT)) props
pushTVar x t (JFunc args res) = JFunc (map (pushTVar x t) args) (pushTVar x t res)

pushTVars :: [(Int, Type)] -> Type -> Type
pushTVars [] t = t
pushTVars ((varName, varType') : vars') t = pushTVar varName varType' (pushTVars vars' t)


setFuncReturnType :: Type -> State Scope (Maybe TypeError)
setFuncReturnType retType = do
  scope <- get
  case funcScope scope of
    Nothing -> return . Just $ TypeError "return outside function scope"
    Just funcScope' -> do
      put $ scope { funcScope = Just $ funcScope' { returnType = retType } }
      return Nothing

isErrExpr :: Expr (VarScope, (Either TypeError Type)) -> Bool
isErrExpr (Expr body (_, Left err)) = True
isErrExpr _ = False

getExprType :: Expr (VarScope, (Either TypeError Type)) -> Maybe Type
getExprType (Expr body (_, Right t)) = Just t
getExprType _ = Nothing

inferType :: VarScope -> Expr a -> State Scope (Expr (VarScope, (Either TypeError Type)))
inferType varScope (Expr body _) = do
  let simply t b = Expr b (varScope, Right t)
  let makeError b str = Expr b (varScope, Left $ TypeError str)
  inferred <- 
      case body of
        LitBoolean x -> return . simply JBoolean $ LitBoolean x
        LitNumber x -> return . simply JNumber $ LitNumber x
        LitString x -> return . simply JString $ LitString x
        LitRegex x -> return . simply JRegex $ LitRegex x
        Var name -> case getVarType varScope name of 
                      Nothing -> return . makeError (Var name) $ "undeclared variable: " ++ name
                      Just varType' -> return . simply varType' $ Var name

        LitArray exprs ->
            do inferredExprs <- forM exprs (inferType varScope)
               let newBody = LitArray inferredExprs
               if any isErrExpr inferredExprs
               then return $ makeError newBody "array elements are badly typed"
               else case map (fromJust . getExprType) inferredExprs of
                 [] -> do elemType <- allocTVar
                          return . simply (JArray elemType) $ LitArray inferredExprs
                 (x:xs) -> if any (/= x) xs
                           then return $ makeError (LitArray inferredExprs) "inconsistent array element types"
                           else return . simply (JArray x) $ LitArray inferredExprs
                 
        LitFunc argNames varNames exprs -> 
            do argScope <- intrVars argNames varScope
               varScope'' <- intrVars varNames argScope
               scope <- get
               returnType' <- allocTVar
               let funcScope' = FuncScope { funcVars = [], returnType = returnType' }
               let (inferredExprs, Scope _ funcScope'') = 
                       flip runState (Scope { typeScope = (typeScope scope), funcScope =  Just funcScope' }) 
                                   $ forM exprs (inferType varScope'')
               if any isErrExpr inferredExprs 
               then return $ makeError (LitFunc argNames varNames inferredExprs) "Error in function body"
               else do
                 let funcType = JFunc (map snd $ vars argScope) (returnType . fromJust $ funcScope'')
                 return . simply funcType $ LitFunc argNames varNames inferredExprs
                 
        Return expr -> do
                  inferredExpr@(Expr _ (_, res)) <- inferType varScope expr
                  case res of 
                    Left _ -> return $ makeError (Return inferredExpr) "Error in return expression"
                    Right retType -> do
                        setFailed <- setFuncReturnType retType
                        case setFailed of
                          Nothing -> return . simply retType $ Return inferredExpr
                          Just _ -> return $ makeError (Return inferredExpr) "Error in return expression"
                  
                  
  return inferred

-- ------------------------------------------------------------------------

ex expr = Expr expr ()

e1 = ex $ LitFunc ["arg"] ["vari"] [ex $ Var "vari", ex $ Return (ex $ LitArray [ex $ Var "arg"])]
t1 = inferType Global e1
s1 = runState t1 emptyScope

-- ------------------------------------------------------------------------



-- inferType :: Expr (Either TypeError Context) -> Expr (Either TypeError Context)
-- inferType e@(Expr _ (Left _)) = e
-- inferType (Expr body (Right ctx)) =
--     case curType ctx of
--       Top -> inferred
--       TVar name -> inferred -- TODO deduce that "name" must be the type given by inferred
--       _ -> if (curType ctx) == (fromJust . getType $ inferred)
--            then inferred
--            else Expr body (Left $ TypeError "type mismatch")
--     where inferred = 
--               case body of 
--                 LitBoolean _ -> rightExpr ctx body JBoolean
--                 LitNumber _ -> rightExpr ctx body JNumber
--                 LitString _ -> rightExpr ctx body JString
--                 LitRegex _ -> rightExpr ctx body JRegex

--                 LitFunc argNames bodyExpr@(Expr funcBody (Right bodyContext)) -> 
--                     Expr newBody funcType
--                     where argTypes = map TVar argNames -- currently type args get value arg names. TODO fix - genreate names
--                           newFuncBody = inferType 
--                                         . withVars (zip argNames argTypes) 
--                                         $ bodyExpr
--                           funcType = either makeBadBody makeFuncType
--                                      $ exprData newFuncBody
--                           makeBadBody = const . Left $ TypeError "func body is badly typed"
--                           makeFuncType bodyContext = Right . newContext $ JFunc argTypes (curType bodyContext)
--                           newBody = LitFunc argNames newFuncBody
--                           newContext t = Context ctx (zip argNames argTypes) t
                                            
--                 LitArray [] -> rightExpr ctx body (JArray $ TVar "todo") -- TODO: generate unique name
--                 LitArray (x:xs) -> 
--                     if isRight headType
--                     then if areSameType 
--                          then rightExpr ctx newBody 
--                                   $ JArray . curType . fromRight 
--                                   $ headType
--                          else makeErrorExpr "could not infer array type: elements are of inconsistent type"
--                     else makeErrorExpr "array head is badly typed"
--                     where headType = exprData . inferSubExprType $ x
--                           inferSubExprType = inferType . withVars (vars ctx)
--                           restTypes = map inferSubExprType xs
--                           areSameType = all ((headType ==) . exprData) restTypes
--                           newBody = LitArray $ (inferType x) : restTypes
--                           makeErrorExpr str = Expr newBody . Left $ TypeError str
                                                
--                 LitObject xs -> 
--                     if any isLeft (map snd propTypes)
--                     then Expr newBody . Left $ TypeError "could not infer object type: properties are badly typed"
--                     else rightExpr ctx newBody 
--                              . JObject 
--                              . map (\(name, expr) -> (name, curType . fromRight $ expr)) 
--                              $ propTypes
--                     where propNamedTypes = map (\(name, expr) -> (name, inferType . withVars (vars ctx) $ expr)) xs
--                           propTypes = map (\(name, expr) -> (name, exprData expr)) propNamedTypes
--                           newBody = LitObject propNamedTypes

--                 Property objExpr propName ->
--                     case inferredObjExpr of
--                       Expr _ (Right (Context _ _ (JObject props))) -> 
--                           case lookup propName props of
--                             Nothing -> makeError $ "object type has no property '" ++ propName ++ "'"
--                             Just propType -> rightExpr ctx newBody propType
--                       _ -> makeError "property accessor on non-object"
--                     where makeError = Expr newBody . Left . TypeError
--                           inferredObjExpr = inferType . withVars (vars ctx) $ objExpr
--                           newBody = Property inferredObjExpr propName

--                 Call callee args ->
--                     case inferredCallee of
--                       Expr _ (Right (Context _ _ (JFunc reqArgTypes returnType))) -> 
--                           if any isLeft inferredArgTypes
--                           then makeError "some arguments are badly typed"
--                           else if any isLeft inferredArgTypes 
--                                then makeError "argument types do not match callee"
--                                else Expr newBody (Right $ Context ctx [] resolvedReturnType)
--                           where coercedArgTypes = map coerceArgTypes
--                                                   $ zip reqArgTypes argTypes
--                                 coerceArgTypes (reqArgType, argType) = 
--                                     case argType of
--                                       Nothing -> Nothing
--                                       Just t -> coerceTypes reqArgType t
--                                 resolvedArgMap = newContextVars coercedArgTypes
--                                 resolvedReturnType = pushTVars resolvedArgMap returnType
--                                 newBody = Call inferredCallee inferredArgs -- TODO


--                       _ -> makeError "call target is not a callable type"

--                     where inferredCallee = inferType  . withVars (vars ctx) $ callee
--                           makeError = Expr (Call inferredCallee inferredArgs) . Left . TypeError
--                           inferredArgs = map inferType args
--                           inferredArgTypes = map exprData inferredArgs
--                           argTypes = map (either (const Nothing) (Just . curType)) inferredArgTypes

--                 Var name -> 
--                     case lookupVar name ctx of
--                       Nothing -> Expr body (Right ctx)
--                       Just t -> Expr body (Right $ Context ctx [] t)

--                 x -> Expr body $ Left $ TypeError ("expression not implemented: " ++ show x)


-- lookupVar :: String -> Context -> Maybe Type
-- lookupVar name Global = Nothing
-- lookupVar name ctx = case lookup name (vars ctx) of
--                        Nothing -> lookupVar name (parent ctx)
--                        Just t -> Just t

-- newContextVars :: [Maybe (Maybe String, Type)] -> [(String, Type)]
-- newContextVars = map (\(maybeName, t) -> (fromJust maybeName, t))
--                  . filter (isJust . fst) 
--                  . map fromJust 
--                  . filter isJust

-- coerceTypes :: Type -> Type -> Maybe (Maybe String, Type)
-- coerceTypes Top x = Just (Nothing, x)
-- coerceTypes x Top = Just (Nothing, x)
-- coerceTypes (TVar s) x = Just (Just s, x)
-- coerceTypes x (TVar s) = Just (Just s, x)
-- coerceTypes _ _ = Nothing

-- fromRight :: (Show a, Show b)=> Either a b -> b
-- fromRight (Right x) = x
-- fromRight e = error $ "Expected Right: " ++ (show e)

-- newExpr :: Body (Expr (Either a Context)) -> Expr (Either a Context)
-- newExpr x = Expr x $ Right $ Context Global [] Top

-- bla = inferType (newExpr (LitArray [newExpr $ LitString "3", newExpr $ LitNumber 3]))
-- blo = inferType (newExpr (LitObject [("test", newExpr $ LitString "3"), ("mest", newExpr $ LitNumber 3)]))
-- blf = inferType . newExpr . LitFunc ["x", "y"] . newExpr $ LitArray [newExpr $ Property (newExpr $ LitString "3") "x"]

-- -- function (x, y) { return [ { x: "bla" }.x ]; }
-- myFunc = newExpr . LitFunc ["x", "y"] . newExpr $ LitArray [newExpr $ Property (newExpr $ LitObject [("a", newExpr $ LitString "bla")]) "a"]

-- blf1 = inferType . newExpr $ Call myFunc [newExpr $ LitString "1", newExpr $ LitString "2"]

-- -- function (x) { return x; }
-- polyFunc = newExpr . LitFunc ["x"] . newExpr $ LitObject [("id", newExpr $ Var "x")]

-- blf2 = inferType . newExpr $ Call polyFunc [newExpr $ LitString "3"]


