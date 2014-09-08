{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Test where

import Types

-- TODO:
-- * Blocks, statements, etc.
-- * Write engine that steps through statements in a program using info to infer types between expressions (e.g. in assignemnts)

import Data.List(intersperse)
import Data.Maybe(fromJust, isJust, isNothing) --, fromMaybe)
import Data.Either(isLeft, lefts, isRight, rights)
import Text.PrettyPrint.GenericPretty(Generic, Out(..), pp)
import Data.Traversable(Traversable(..))
import Data.Foldable(Foldable(..))
import Control.Monad.State(State, runState, forM, get, put)
import qualified Data.Map.Lazy as Map
import Prelude hiding (foldr, mapM)
import Control.Monad()

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
               | Property expr String  -- lvalue must be a JSObject
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
      Assign target src -> (toJs target) ++ " = " ++ (toJs src)
      Call callee args -> (toJs callee) ++ "(" ++ (commafy $ map toJs args) ++ ")"
      Index arr idx -> (toJs arr) ++ "[" ++ (toJs idx) ++ "]"
      LitArray xs -> "[ " ++ (commafy $ map toJs xs) ++ " ]"
      LitBoolean x -> if x then "true" else "false"
      LitFunc args varNames exprs -> "function (" ++ argsJs ++ ") " ++ block
          where argsJs = commafy $ args
                block =  "{\n" ++ vars' ++ "\n" ++ statements ++ " }\n"
                statements = (concat $ map (++ ";\n") $ map toJs exprs)
                vars' = "var " ++ commafy varNames ++ ";"
      LitNumber x -> show x
      LitObject xs -> "{ " ++ (commafy $ map (\(name, val) -> name ++ ": " ++ (toJs val)) xs) ++ " }"
      LitRegex regex -> "/" ++ regex ++ "/" -- todo correctly
      LitString s -> "'" ++ s ++ "'" -- todo escape
      Property obj name -> (toJs obj) ++ "." ++ name
      Return expr -> "return " ++ toJs expr
      Var name -> name


data TypeError = TypeError String
               deriving (Show, Eq, Generic)

instance Out TypeError


data VarScope = Global | VarScope { parent :: VarScope, vars :: [(String, JSType)] }
               deriving (Show, Eq, Generic)

instance Out VarScope

instance (Out k, Out v) => Out (Map.Map k v) where
    doc m = doc $ Map.assocs m
    docPrec _ = doc

data TypeScope = TypeScope { tVars :: TSubst JSConsType, maxNum :: Int }
               deriving (Show, Eq, Generic)

instance Out TypeScope

data FuncScope = FuncScope { funcVars :: [(String, JSType)]
                           , returnType :: JSType }
               deriving (Show, Eq, Generic)

instance Out FuncScope


data Scope = Scope { typeScope :: TypeScope
                   , funcScope :: Maybe FuncScope }
               deriving (Show, Eq, Generic)

instance Out Scope

getVarType :: VarScope -> String -> Maybe JSType
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

allocTVar' :: TypeScope -> (JSType, TypeScope)
allocTVar' tscope = (JSTVar allocedNum, updatedScope)
    where updatedScope = tscope { maxNum = allocedNum }
          allocedNum = (maxNum tscope) + 1


allocTVar :: State Scope JSType
allocTVar = do
  scope <- get
  let typeScope' = typeScope scope
      (varType', typeScope'') = allocTVar' typeScope'
  put $ scope { typeScope = typeScope'' }
  return varType'


emptyTypeScope :: TypeScope
emptyTypeScope = TypeScope Map.empty 0

emptyScope :: Scope
emptyScope = Scope { typeScope = emptyTypeScope, funcScope = Nothing }

-- rightExpr :: Scope -> Body (Expr (Scope, (Either a b))) -> b -> Expr (Scope, (Either a b))
-- rightExpr scope body x = Expr body (scope, Right x)

exprData :: Expr t -> t
exprData (Expr _ t) = t


getFuncReturnType :: State Scope (Maybe JSType)
getFuncReturnType = do
  scope <- get
  case funcScope scope of
    Nothing -> return Nothing
    Just funcScope' -> return . Just $ returnType funcScope'

setFuncReturnType :: JSType -> State Scope (Maybe TypeError)
setFuncReturnType retType = do
  scope <- get
  case funcScope scope of
    Nothing -> return . Just $ TypeError "return outside function scope"
    Just funcScope' -> do
      put $ scope { funcScope = Just $ funcScope' { returnType = retType } }
      return Nothing

isErrExpr :: Expr (VarScope, (Either TypeError JSType)) -> Bool
isErrExpr (Expr _ (_, Left _)) = True
isErrExpr _ = False

getExprType :: Expr (VarScope, (Either TypeError JSType)) -> Maybe JSType
getExprType (Expr _ (_, Right t)) = Just t
getExprType _ = Nothing


coerceTypes :: JSType -> JSType -> State Scope (Either TypeError JSType)
coerceTypes t u = do
  scope <- get
  let typeScope' = typeScope scope
  let tsubst = tVars typeScope'
  case unify tsubst (toType t) (toType u) of
    Nothing -> return . Left . TypeError $ "Failed unifying types: " ++ (show t) ++ " and " ++ (show u)
    Just x -> do
      let tsubst' = x
      let scope' = scope { typeScope = typeScope' { tVars = tsubst' } }
      put scope'
      return . Right . fromType $ substituteType tsubst' (toType t)

resolveType :: JSType -> State Scope JSType
resolveType t = do
  scope <- get
  let typeScope' = typeScope scope
  let tsubst = tVars typeScope'
  return . fromType $ substituteType tsubst (toType t)

type InferredExpr = Expr (VarScope, (Either TypeError JSType))

inferType' :: VarScope -> Expr a -> State Scope (Maybe JSType)
inferType' v e = do
  inferredExpr <- inferType v e
  case inferredExpr of
    Expr _ (_, Left _) -> return Nothing
    Expr _ (_, Right t) ->
      do t' <- resolveType t
         return . Just $ t'
  

inferType :: VarScope -> Expr a -> State Scope InferredExpr
inferType varScope (Expr body _) = do
  case body of
    LitArray exprs -> inferArrayType varScope exprs
    LitBoolean x -> simpleType JSBoolean $ LitBoolean x
    LitFunc argNames varNames exprs -> inferFuncType varScope argNames varNames exprs
    LitNumber x -> simpleType JSNumber $ LitNumber x
    LitObject props -> inferObjectType varScope props
    LitRegex x -> simpleType JSRegex $ LitRegex x
    LitString x -> simpleType JSString $ LitString x
    Return expr -> inferReturnType varScope expr
    Var name -> inferVarType varScope name
    Call callee args -> inferCallType varScope callee args
                  
  where simpleType t body' = return $ simply varScope t body'

        
simply ::  v -> t -> Body (Expr (v, Either a t)) -> Expr (v, Either a t)
simply varScope t b = Expr b (varScope, Right t)

makeError' :: t -> Body (Expr (t, Either a b)) -> a -> Expr (t, Either a b)
makeError' varScope b typeError = Expr b (varScope, Left typeError)

makeError :: v -> Body (Expr (v, Either TypeError b)) -> String -> Expr (v, Either TypeError b)
makeError varScope b str = makeError' varScope b $ TypeError str

inferCallType :: VarScope -> Expr a -> [Expr a] -> State Scope InferredExpr
inferCallType varScope callee args = do
  inferredCallee <- inferType varScope callee
  inferredArgs <- mapM (inferType varScope) args
  let newBody = Call inferredCallee inferredArgs
  case getExprType inferredCallee of
    Nothing -> return . makeError varScope newBody $ "couldn't infer callee in call expression"
    Just (JSFunc argsT' res') -> do
      let maybeArgsT = map getExprType inferredArgs
      if any isNothing maybeArgsT
      then return . makeError varScope newBody $ "couldn't infer arg types in call expression"
      else do
        let argsT = map fromJust maybeArgsT
        unifiedArgTypes <- mapM (uncurry coerceTypes) $ zip argsT argsT'
        if any isLeft unifiedArgTypes
        then return . makeError varScope newBody $ "actual argument types do not match callee argument types:" 
                 ++ (concat $ intersperse "\n" (map show $ lefts unifiedArgTypes))
        else return $ simply varScope res' newBody
    Just _ -> return . makeError varScope newBody $ "callee is not a function"
  
inferVarType :: VarScope -> String -> State Scope InferredExpr
inferVarType varScope name = do
  case getVarType varScope name of 
    Nothing -> return . makeError varScope (Var name) $ "undeclared variable: " ++ name
    Just varType' -> return . simply varScope varType' $ Var name

inferArrayType :: VarScope -> [Expr a] -> State Scope InferredExpr
inferArrayType varScope exprs = 
    do inferredExprs <- forM exprs (inferType varScope)
       let newBody = LitArray inferredExprs
       if any isErrExpr inferredExprs
       then return $ makeError varScope newBody "array elements are badly typed"
       else case map (fromJust . getExprType) inferredExprs of
              [] -> do elemType <- allocTVar
                       return . simply varScope (JSArray elemType) $ LitArray inferredExprs
              (x:xs) -> if any (/= x) xs
                        then return $ makeError varScope (LitArray inferredExprs) "inconsistent array element types"
                        else return . simply varScope (JSArray x) $ LitArray inferredExprs

inferFuncType :: VarScope -> [String] -> [String] -> [Expr a] -> State Scope InferredExpr
inferFuncType varScope argNames varNames exprs =
    do argScope <- intrVars argNames varScope
       _ <- allocTVar
       varScope'' <- intrVars varNames argScope
       returnType' <- allocTVar
       scope <- get
       let funcScope' = FuncScope { funcVars = [], returnType = returnType' }
       let (inferredExprs, Scope typeScope'' funcScope'') = 
               flip runState (Scope { typeScope = (typeScope scope), funcScope =  Just funcScope' }) 
                    $ forM exprs (inferType varScope'')
       put $ scope { typeScope = typeScope'' }
       if any isErrExpr inferredExprs 
       then return $ makeError varScope(LitFunc argNames varNames inferredExprs) "Error in function body"
       else do
         let funcType = JSFunc (map snd $ vars argScope) (returnType . fromJust $ funcScope'')
         return . simply varScope funcType $ LitFunc argNames varNames inferredExprs

inferReturnType :: VarScope -> Expr a -> State Scope InferredExpr
inferReturnType varScope expr =
    do inferredExpr@(Expr _ (_, res)) <- inferType varScope expr
       let newBody = Return inferredExpr
       case res of 
         Left _ -> return $ makeError varScope newBody "Error in return expression"
         Right retType -> 
             do curReturnType <- getFuncReturnType
                if isJust curReturnType
                then do
                  maybeT <- coerceTypes retType $ fromJust curReturnType
                  case maybeT of
                    Left e -> return $ makeError' varScope newBody e
                    Right t -> do
                                setFailed <- setFuncReturnType t
                                case setFailed of
                                  Nothing ->  return . simply varScope t $ newBody
                                  Just _ -> return $ makeError varScope newBody "Error in return expression"
                else do setFailed <- setFuncReturnType retType
                        case setFailed of
                          Nothing -> return . simply varScope retType $ newBody
                          Just _ -> return $ makeError varScope newBody "Error in return expression"
 
inferObjectType :: VarScope -> [(String, Expr a)] -> State Scope InferredExpr
inferObjectType varScope props =
    do let propNames = map fst props
       let propExprs = map snd props
       inferredProps <- mapM (inferType varScope) propExprs
       let newBody = LitObject $ zip propNames inferredProps
       if any isErrExpr inferredProps
       then return $ makeError varScope newBody "object properties are badly typed"
       else return 
                . simply varScope (JSObject 
                                  $ zip propNames 
                                  $ map (fromJust . getExprType) inferredProps) 
                      $ newBody

-- ------------------------------------------------------------------------

ex expr = Expr expr ()

e1 = ex $ LitFunc ["arg"] ["vari"] [ex $ Var "vari"
                                   , ex $ Return (ex $ LitArray [])
                                   , ex $ Return (ex $ LitArray [ex $ LitObject [("bazooka", ex $ Var "arg")]])]
--e1 = ex $ LitFunc ["arg"] ["vari"] []

t1 = inferType' Global e1
s1 = fst $ runState t1 emptyScope

e2 = ex $ Call e1 [(ex $ LitString "abc")]
s2 = fst $ runState (inferType' Global e2) emptyScope