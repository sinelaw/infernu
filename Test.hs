{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Test where

import Types

-- TODO:
-- * Blocks, statements, etc.
-- * Write engine that steps through statements in a program using info to infer types between expressions (e.g. in assignemnts)

--import Data.List(intersperse)
import Data.Maybe(fromJust, isJust) --, fromMaybe)
--import Data.Either(isLeft, lefts, isRight)
import Text.PrettyPrint.GenericPretty(Generic, Out(..), pp)
import Data.Traversable(Traversable(..))
import Data.Foldable(Foldable(..))
import Control.Monad.State(State, runState, forM, get, put)
import qualified Data.Map.Lazy as Map
import Prelude hiding (foldr, mapM)
import Control.Monad(join)

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


data VarScope = Global | VarScope { parent :: VarScope, vars :: [(String, JSType)] }
               deriving (Show, Eq, Generic)

instance Out VarScope

instance (Out k, Out v) => Out (Map.Map k v) where
    doc m = doc $ Map.assocs m
    docPrec _ = doc

data TypeScope = TypeScope { tVars :: TSubst JSConsType }
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
    where updatedScope = TypeScope { tVars = tVars' }
          oldTVars = tVars tscope
          tVars' = fromJust $ extend oldTVars allocedNum (TVar allocedNum)
          maxNum = foldr max 0 $ Map.keys oldTVars
          allocedNum = maxNum + 1


allocTVar :: State Scope JSType
allocTVar = do
  scope <- get
  let typeScope' = typeScope scope
      (varType', typeScope'') = allocTVar' typeScope'
  put $ scope { typeScope = typeScope'' }
  return varType'


emptyTypeScope :: TypeScope
emptyTypeScope = TypeScope Map.empty

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


inferType :: VarScope -> Expr a -> State Scope (Expr (VarScope, (Either TypeError JSType)))
inferType varScope (Expr body _) = do
  let simply t b = Expr b (varScope, Right t)
  let makeError' b typeError = Expr b (varScope, Left typeError)
  let makeError b str = makeError' b $ TypeError str
  inferred <- 
      case body of
        LitBoolean x -> return . simply JSBoolean $ LitBoolean x
        LitNumber x -> return . simply JSNumber $ LitNumber x
        LitString x -> return . simply JSString $ LitString x
        LitRegex x -> return . simply JSRegex $ LitRegex x
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
                          return . simply (JSArray elemType) $ LitArray inferredExprs
                 (x:xs) -> if any (/= x) xs
                           then return $ makeError (LitArray inferredExprs) "inconsistent array element types"
                           else return . simply (JSArray x) $ LitArray inferredExprs
                 
        LitFunc argNames varNames exprs -> 
            do argScope <- intrVars argNames varScope
               varScope'' <- intrVars varNames argScope
               scope <- get
               returnType' <- allocTVar
               let funcScope' = FuncScope { funcVars = [], returnType = returnType' }
               let (inferredExprs, Scope typeScope'' funcScope'') = 
                       flip runState (Scope { typeScope = (typeScope scope), funcScope =  Just funcScope' }) 
                                   $ forM exprs (inferType varScope'')
               put $ scope { typeScope = typeScope'' }
               if any isErrExpr inferredExprs 
               then return $ makeError (LitFunc argNames varNames inferredExprs) "Error in function body"
               else do
                 let funcType = JSFunc (map snd $ vars argScope) (returnType . fromJust $ funcScope'')
                 return . simply funcType $ LitFunc argNames varNames inferredExprs
                 
        Return expr -> 
            do inferredExpr@(Expr _ (_, res)) <- inferType varScope expr
               let newBody = Return inferredExpr
               case res of 
                 Left _ -> return $ makeError newBody "Error in return expression"
                 Right retType -> 
                     do curReturnType <- getFuncReturnType
                        if isJust curReturnType
                        then do
                          maybeT <- coerceTypes retType $ fromJust curReturnType
                          case maybeT of
                            Left e -> return $ makeError' newBody e
                            Right t -> do
                                     setFailed <- setFuncReturnType t
                                     case setFailed of
                                       Nothing ->  return . simply t $ newBody
                                       Just _ -> return $ makeError newBody "Error in return expression"
                        else do setFailed <- setFuncReturnType retType
                                case setFailed of
                                  Nothing -> return . simply retType $ newBody
                                  Just _ -> return $ makeError newBody "Error in return expression"
                  
        LitObject props -> 
            do let propNames = map fst props
               let propExprs = map snd props
               inferredProps <- mapM (inferType varScope) propExprs
               let newBody = LitObject $ zip propNames inferredProps
               if any isErrExpr inferredProps
               then return $ makeError newBody "object properties are badly typed"
               else return 
                        . simply (JSObject 
                                  $ zip propNames 
                                  $ map (fromJust . getExprType) inferredProps) 
                        $ newBody
                  
  return inferred

-- ------------------------------------------------------------------------

ex expr = Expr expr ()

e1 = ex $ LitFunc ["arg"] ["vari"] [ex $ Var "vari"
                                   , ex $ Return (ex $ LitArray [])
                                   , ex $ Return (ex $ LitArray [ex $ LitObject [("bazooka", ex $ Var "arg")]])]
t1 = inferType Global e1
s1 = runState t1 emptyScope

