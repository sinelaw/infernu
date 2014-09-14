{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Infer where

import Types
--import Pretty

-- TODO:
-- 
-- * improve pretty print by resolving all types deeply before printing (avoid type vars)
-- 
-- * infer record types by their usage in a function
-- 
-- * support 'this' by assuming equivalences:
--   - f(..) == f.bind(window, ..)
--   - o.f(..) == f.bind(o, ..)
--
-- * support new (e.g. add body Constructor ... that's like a func
-- 
-- * check zips for missing errors (zip of lists of different sizes)
--
-- * don't allow assigning to function args? (lint issue only)

import Debug.Trace
import Data.List(intersperse)
import Data.Maybe(fromJust, isJust, isNothing) --, fromMaybe)
import Data.Either(isLeft, lefts)
import Text.PrettyPrint.GenericPretty(Generic)
import Control.Monad.State(State, runState, forM, get, put)
import Data.Traversable(Traversable(..))
import Data.Foldable(Foldable(..))
import qualified Data.Map.Lazy as Map
import Prelude hiding (foldr, mapM)
import Control.Monad()

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left x) = error $ "expected: Right _, got: Left " ++ show x

-- getVarType = bad name, because it isn't = lookup name . getVars
getVarType :: VarScope -> String -> Maybe JSType
getVarType Global _ = Nothing
getVarType scope name = case lookup name (vars scope) of
                       Nothing -> getVarType (parent scope) name
                       Just t -> Just t

getVars :: VarScope -> [(String, JSType)]
getVars Global = []
getVars scope = vars scope

intrVars :: [String] -> State Scope VarScope
intrVars names = do
  scope <- get
  let varScope' = varScope scope
  vs <- forM names $ \name -> do
          varType' <-  allocTVar
          return (name, varType')

  return VarScope { parent = varScope', vars = vs }

intrVarWithType :: String -> JSType -> State Scope VarScope
intrVarWithType name t = do
  scope <- get
  let varScope' = varScope scope
  return VarScope { parent = varScope', vars = (name, t) : getVars varScope' }


updateVarScope :: VarScope -> State Scope ()
updateVarScope v = do
  scope <- get
  put $ scope { varScope = v }

allocTVar' :: TypeScope -> (JSType, TypeScope)
allocTVar' tscope = (JSTVar allocedNum, updatedScope)
    where updatedScope = tscope { maxNum = allocedNum }
          allocedNum = maxNum tscope + 1


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
emptyScope = Scope { typeScope = emptyTypeScope, funcScope = Nothing, varScope = Global }

exprData :: Expr t -> t
exprData (Expr _ t) = t

exprBody :: Expr t -> Body (Expr t)
exprBody (Expr b _) = b


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

isErrExpr :: InferredExpr -> Bool
isErrExpr (Expr _ (Left _)) = True
isErrExpr _ = False

getExprResult :: InferredExpr -> Either TypeError JSType
getExprResult (Expr _ result) = result

getExprType :: InferredExpr -> Maybe JSType
getExprType (Expr _ (Right t)) = Just t
getExprType _ = Nothing

getExprError :: InferredExpr -> Maybe TypeError
getExprError (Expr _ (Left e)) = Just e
getExprError _ = Nothing

coerceTypes :: JSType -> JSType -> State Scope (Either TypeError JSType)
coerceTypes t u = do
  scope <- get
  let typeScope' = typeScope scope
  let tsubst = tVars typeScope'
  case unify tsubst (toType t) (toType u) of
    Left e -> return . Left . TypeError $ "Failed unifying types: " ++ show t ++ " and " ++ show u ++ " - error: " ++ e
    Right x -> do
      let tsubst' = x
      let scope' = scope { typeScope = typeScope' { tVars = tsubst' } }
      put scope'
      return . Right . fromType $ substituteType tsubst' (toType t)

resolveType :: JSType -> State Scope (Maybe JSType)
resolveType t = do
  scope <- get
  let typeScope' = typeScope scope
  let tsubst = tVars typeScope'
  let subst' t' = case substituteType tsubst t' of
                    t''@(TVar _) -> if t' == t'' then Just t'
                                    else subst' t''
                    TCons consName ts -> 
                        let substTS = map subst' ts in
                        if any isNothing substTS
                        then Nothing
                        else Just $ TCons consName (map fromJust substTS)
      result = fmap fromType . subst' $ toType t
  return (traceShow (result, t) result)

inferStatement ::  Statement (Expr a) -> State Scope InferredStatement
inferStatement st = do
  let ok st' = return $ Right st'
      err st' e = return $ Left (e, st')
  case st of
    Empty -> ok Empty

    Expression expr ->
        do inferredExpr <- inferType expr
           let newSt = Expression inferredExpr
           case getExprResult inferredExpr of
             Left e -> err newSt e
             Right _ -> ok newSt 
            
    Block xs -> 
        do results <- mapM inferStatement xs
           let newSt = Block $ map getInferredStatement results
           case lefts results of 
             [] -> ok newSt
             _ -> err newSt $ TypeError "error in statement block"

    IfThenElse expr stThen stElse ->
        do inferredExpr <- inferType expr
           stThen' <- inferStatement stThen
           stElse' <- inferStatement stElse
           let stThen'' = getInferredStatement stThen'
               stElse'' = getInferredStatement stElse'
               newSt = IfThenElse inferredExpr stThen'' stElse''
           case getExprResult inferredExpr of
             Left e -> err newSt e
             Right t -> 
                 do coercedPredType <- coerceTypes t JSBoolean
                    case (coercedPredType, stThen' , stElse') of
                      (Right _, Right _, Right _) -> ok newSt
                      _ -> err newSt $ TypeError "error in if-then-else"

    Return Nothing -> 
        do returnT <- getFuncReturnType
           case returnT of
             Nothing -> trySetReturnType JSUndefined
             Just returnT' -> 
                 do t <- coerceTypes returnT' JSUndefined
                    case t of
                      Left e -> err newSt e
                      Right t' -> trySetReturnType t'

        where newSt = Return Nothing
              trySetReturnType t = do
               returnT' <- setFuncReturnType t
               case returnT' of
                 Nothing -> ok newSt
                 Just e -> err newSt e


    Return (Just expr) -> 
        do inferredExpr <- inferReturnType expr
           let newSt = Return $ Just inferredExpr
           case getExprResult inferredExpr of
             Left e -> err newSt e
             Right _ -> ok newSt

    While expr stWhile ->
        do inferredExpr <- inferType expr
           inferredStWhile <- inferStatement stWhile
           let inferredStWhile' = getInferredStatement inferredStWhile
               newSt = While inferredExpr inferredStWhile'
           case getExprResult inferredExpr of
             Left e -> err newSt e
             Right t -> 
                 do coercedPredType <- coerceTypes t JSBoolean
                    case (coercedPredType, inferredStWhile) of 
                      (Right _, Right _) -> ok newSt
                      _ -> err newSt $ TypeError "error in while statment"

    VarDecl name ->
        do updatedVarScope <- intrVars [name]
           scope <- get
           put $ scope { varScope = updatedVarScope }
           ok $ VarDecl name

type InferredResult = Either TypeError JSType
type InferredStatement = Either (TypeError, Statement InferredExpr) (Statement InferredExpr)
type InferredExpr = Expr InferredResult

getInferredStatement :: Either (a, b) b -> b
getInferredStatement (Left (_, x)) = x
getInferredStatement (Right x) = x


resolve :: InferredExpr -> State Scope InferredExpr
resolve ie = 
    case ie of
      Expr _ (Left _) -> return ie
      Expr a (Right t) ->
          do t' <- resolveType t
             case t' of
               Nothing -> return $ Expr a (Left . TypeError $ "recursive type")
               Just t'' -> return $ Expr a (Right t'')

inferType ::  Expr a -> State Scope InferredExpr
inferType e = do
  inferredExpr <- inferType' e
  --traverse resolve inferredExpr
  resolve inferredExpr

inferType' ::   Expr a -> State Scope InferredExpr
inferType' (Expr body _) =
  case body of
    LitArray exprs -> inferArrayType exprs
    LitBoolean x -> simpleType JSBoolean $ LitBoolean x
    LitFunc name argNames exprs -> inferFuncType name argNames exprs
    LitNumber x -> simpleType JSNumber $ LitNumber x
    LitObject props -> inferObjectType props
    LitRegex x -> simpleType JSRegex $ LitRegex x
    LitString x -> simpleType JSString $ LitString x
    Var name -> inferVarType name
    Call callee args -> inferCallType callee args
    Assign dest src -> inferAssignType dest src
    Property expr name -> inferPropertyType expr name
    Index arrExpr indexExpr -> inferIndexType arrExpr indexExpr
  where simpleType t body' = return $ simply t body'

        
simply :: t -> Body (Expr (Either a t)) -> Expr (Either a t)
simply t b = Expr b (Right t)

makeError' :: Body (Expr (Either a b)) -> a -> Expr (Either a b)
makeError' b typeError = Expr b (Left typeError)

makeError :: Body (Expr (Either TypeError b)) -> String -> Expr (Either TypeError b)
makeError b str = makeError' b $ TypeError str

inferIndexType :: Expr a -> Expr a  -> State Scope InferredExpr
inferIndexType arrExpr indexExpr = do
  inferredArrExpr <- inferType arrExpr
  inferredIndexExpr <- inferType indexExpr
  let newBody = Index inferredArrExpr inferredIndexExpr
  if any isNothing $ map getExprType [inferredArrExpr, inferredIndexExpr]
  then return . makeError newBody $ "couldn't infer index target or value"
  else do
    let arrType = getExprType inferredArrExpr
        indexType = getExprType inferredIndexExpr
        
    case (arrType, indexType) of
      (Just (JSArray elemType), Just JSNumber) -> return $ simply elemType newBody
      _ -> return . makeError newBody $ "Left-hand side of index is not an array or right-hand side is not a number"
    

inferAssignType :: Expr a -> Expr a -> State Scope InferredExpr
inferAssignType dest src = do
  inferredDest <- inferType dest
  inferredSrc <- inferType src
  let newBody = Assign inferredDest inferredSrc
  if any isNothing $ map getExprType [inferredSrc, inferredDest]
  then return . makeError newBody $ "couldn't infer left or right of assign statement"
  else do 
    let destType = fromJust . getExprType $ inferredDest
        srcType = fromJust . getExprType $ inferredSrc
        infer' = do
          varType <- coerceTypes destType srcType
          case varType of 
            Left err -> return . makeError' newBody $ err
            Right varType' -> return $ simply varType' newBody
    case exprBody inferredDest of
      Var _ -> infer'
      Property _ _ -> infer' -- TODO update object type?
      _ -> return . makeError newBody $ "Left-hand side of assignment is not an lvalue"

inferPropertyType :: Expr a -> String -> State Scope InferredExpr
inferPropertyType objExpr propName =
    do inferredObjExpr <- inferType objExpr
       let newBody = Property inferredObjExpr propName
           objType = getExprType inferredObjExpr
       case objType of
         Nothing -> return . makeError newBody $ "failed inferring object type"
         Just objType' ->
           case getObjPropertyType objType' propName of
                Nothing -> return . makeError newBody $ ("object type has no property named '" ++ propName ++ "'")
                Just propType' -> return $ simply propType' newBody

inferCallType :: Expr a -> [Expr a] -> State Scope InferredExpr
inferCallType callee args = do
  inferredCallee <- inferType callee
  inferredArgs <- mapM inferType args
  callResultType <- allocTVar
  let newBody = Call inferredCallee inferredArgs
      inferredCalleeType = getExprType inferredCallee
  case inferredCalleeType of
    Nothing -> return . makeError newBody $ "couldn't infer callee in call expression, or isn't a function"
    Just t -> 
        do let argTypes = map getExprType inferredArgs
           if any isNothing argTypes
           then return . makeError newBody $ "couldn't infer arg types in call expression"
           else do funcType' <- coerceTypes t (JSFunc (map fromJust argTypes) callResultType)
                   case funcType' of
                     Left (TypeError e) -> return $ makeError newBody e
                     Right (JSFunc _ returnType') -> return $ simply returnType' newBody
  
inferVarType :: String -> State Scope InferredExpr
inferVarType name = do
  scope <- get
  let varType = getVarType (varScope scope) name
  case varType of 
    Nothing -> return . makeError (Var name) $ "undeclared variable: " ++ name
    Just varType' -> return . simply varType' $ Var name

inferArrayType :: [Expr a] -> State Scope InferredExpr
inferArrayType exprs = 
    do inferredExprs <- forM exprs inferType
       let newBody = LitArray inferredExprs
       if any isErrExpr inferredExprs
       then return $ makeError newBody "array elements are badly typed"
       else case map (fromJust . getExprType) inferredExprs of
              [] -> do elemType <- allocTVar
                       return . simply (JSArray elemType) $ LitArray inferredExprs
              (x:xs) -> if any (/= x) xs
                        then return $ makeError (LitArray inferredExprs) "inconsistent array element types"
                        else return . simply (JSArray x) $ LitArray inferredExprs

inferFuncType :: Maybe String -> [String] -> [Statement (Expr a)] -> State Scope InferredExpr
inferFuncType name argNames exprs =
    do returnType' <- allocTVar
       -- if the function has a name, introduce it as a variable to the var context before the argument names
       funcType <- 
           case name of
             Just x -> do varScope' <- intrVars [x]
                          updateVarScope varScope'
                          return . snd . head . getVars $ varScope'
             Nothing -> allocTVar
       -- allocate type variables for the arguments
       argScope <- intrVars argNames
       let funcScope' = FuncScope { returnType = returnType' }
           argTypes = map snd $ getVars argScope
       scope <- get
       -- infer the statements that make up this function
       let (inferredStatments', Scope typeScope'' _ funcScope'') = 
               flip runState (scope { funcScope = Just funcScope', varScope = traceShowId argScope }) 
                    $ forM exprs inferStatement
           inferredStatments = (map getInferredStatement inferredStatments')
       -- update scope with type/unification changes from the statements' processing
       put $ scope { typeScope = typeScope'' }
       let newBody = LitFunc name argNames inferredStatments
       case (any isLeft inferredStatments', funcScope'') of
         (False, Just updatedFuncScope) -> 
             do let inferredReturnType = returnType updatedFuncScope
                unifiedReturnType <- coerceTypes (traceShowId returnType') (traceShowId inferredReturnType)
                unifiedFuncType' <- 
                    case unifiedReturnType of
                      Left _ -> return unifiedReturnType
                      Right unifiedReturnType'' -> coerceTypes funcType . JSFunc argTypes $ unifiedReturnType''
                case unifiedFuncType' of
                  Left _ -> return $ makeError newBody "Error inferring function type"
                  Right t -> return $ simply t newBody
         _ -> return $ makeError newBody "Error in function body"

inferReturnType ::  Expr a -> State Scope InferredExpr
inferReturnType expr =
    do (Expr newBody res) <- inferType expr
       case res of 
         Left (TypeError e) -> return $ makeError newBody e
         Right retType -> 
             do curReturnType <- getFuncReturnType
                if isJust curReturnType
                then do
                  maybeT <- coerceTypes retType $ fromJust curReturnType
                  case maybeT of
                    Left e -> return $ makeError' newBody e
                    Right t -> setRetType t
                else setRetType retType
             where setRetType retType' = 
                       do setFailed <- setFuncReturnType retType'
                          case setFailed of
                            Nothing -> return . simply retType' $ newBody
                            Just _ -> return $ makeError newBody "Error in return expression"
 
inferObjectType :: [(String, Expr a)] -> State Scope InferredExpr
inferObjectType props =
    do let propNames = map fst props
       let propExprs = map snd props
       inferredProps <- mapM inferType propExprs
       let newBody = LitObject $ zip propNames inferredProps
       if any isErrExpr inferredProps
       then return $ makeError newBody "object properties are badly typed"
       else return 
                . simply (JSObject 
                                  $ zip propNames 
                                  $ map (fromJust . getExprType) inferredProps) 
                      $ newBody


