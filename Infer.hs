{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Infer where

import Types
--import Pretty

-- TODO:
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

--import Control.Error
import Data.Maybe(fromJust, isJust, isNothing) --, fromMaybe)
import Data.Either(isLeft, lefts)
import Control.Monad(join)
import Control.Monad.State(State, runState, forM, get, put)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Trans.Either(EitherT(..), left)
import Data.Traversable(Traversable(..))
import qualified Data.Map.Lazy as Map
import Prelude hiding (foldr, mapM)

type InferredStatement = Statement InferredExpr
type InferredExpr = Expr JSType


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

toEither :: err -> Maybe a -> Either err a
toEither err Nothing = Left err
toEither _ (Just x) = Right x

getFuncScope :: Scope -> Either TypeError FuncScope
getFuncScope = toEither (TypeError "Not in a function scope") . funcScope

getFuncReturnType :: EitherT TypeError (State Scope) JSType
getFuncReturnType = do
  funcScope' <- EitherT $ fmap getFuncScope get
  return $ returnType funcScope'

setFuncReturnType :: JSType -> EitherT TypeError (State Scope) ()
setFuncReturnType retType = do
  scope <- get
  funcScope' <- EitherT . return $ getFuncScope scope
  put $ scope { funcScope = Just $ funcScope' { returnType = retType } }

coerceTypes :: JSType -> JSType -> EitherT TypeError (State Scope) JSType
coerceTypes t u = do
  scope <- get
  let typeScope' = typeScope scope
  let tsubst = tVars typeScope'
  tsubst' <- EitherT . return $ unify tsubst (toType t) (toType u)
  put scope { typeScope = typeScope' { tVars = tsubst' } }
  return . fromType $ substituteType tsubst' (toType t)

resolveType :: TypeScope -> JSType -> JSType
resolveType ts t = fromType . subst' $ toType t
  where tsubst = tVars ts
        subst' t' = case substituteType tsubst t' of
                      t''@(TVar _) -> if t' == t'' then t'
                                      else subst' t''
                      TCons consName ts -> 
                          let substTS = map subst' ts in
                          TCons consName substTS


--ok = return

err :: Monad m => b -> a -> m (Either (a, b) c)
err st' e = return $ Left (e, st')


inferStatement ::  Statement (Expr a) -> EitherT TypeError (State Scope) InferredStatement
inferStatement st =
  case st of
    Empty -> return Empty
    Expression expr -> inferExprStatement expr
    Block xs -> inferBlockStatement xs
    IfThenElse expr stThen stElse -> inferIfThenElse  expr stThen stElse 
    Return Nothing -> inferReturnNothing
    Return (Just expr) -> inferReturnExpr expr
    While expr stWhile -> inferWhile expr stWhile
    VarDecl name -> lift $ inferVarDecl name

inferVarDecl :: String -> State Scope InferredStatement
inferVarDecl name =         
    do updatedVarScope <- intrVars [name]
       scope <- get
       put $ scope { varScope = updatedVarScope }
       return $ VarDecl name

inferWhile :: Expr a -> Statement (Expr a) -> EitherT TypeError (State Scope) InferredStatement
inferWhile expr stWhile =
    do inferredExpr <- inferType expr
       inferredStWhile <- inferStatement stWhile
       let newSt = While inferredExpr inferredStWhile
       coerceTypes (exprData inferredExpr) JSBoolean
       return newSt

inferReturnExpr :: Expr a -> EitherT TypeError (State Scope) InferredStatement
inferReturnExpr expr =
    do inferredExpr <- inferReturnType expr
       return . Return $ Just inferredExpr

inferReturnNothing :: EitherT TypeError (State Scope) InferredStatement
inferReturnNothing = 
    do returnT <- getFuncReturnType
       t <- coerceTypes returnT JSUndefined
       setFuncReturnType t
       return $ Return Nothing

inferIfThenElse :: Expr a -> Statement (Expr a) -> Statement (Expr a) -> EitherT TypeError (State Scope) InferredStatement
inferIfThenElse expr stThen stElse = 
    do inferredExpr <- inferType expr
       stThen' <- inferStatement stThen
       stElse' <- inferStatement stElse
       let stThen'' = stThen'
           stElse'' = stElse'
           newSt = IfThenElse inferredExpr stThen'' stElse''
       coercedPredType <- coerceTypes (exprData inferredExpr) JSBoolean
       return newSt

inferExprStatement :: Expr a -> EitherT TypeError (State Scope) InferredStatement
inferExprStatement expr = 
    do inferredExpr <- inferType expr
       return $ Expression inferredExpr 

inferBlockStatement :: [Statement (Expr a)] -> EitherT TypeError (State Scope) InferredStatement
inferBlockStatement xs =
    do results <- mapM inferStatement xs
       return $ Block results

inferType :: Expr a -> EitherT TypeError (State Scope) InferredExpr
inferType e = do
  inferredExpr <- inferType' e
  scope <- lift get
  return $ fmap (resolveType $ typeScope scope) inferredExpr
  --resolve inferredExpr

inferType' ::   Expr a -> EitherT TypeError (State Scope) InferredExpr
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
  where simpleType t body' = return $ Expr body' t

        
simply :: t -> Body (Expr (Either a t)) -> Expr (Either a t)
simply t b = Expr b (Right t)

makeError' :: Body (Expr (Either a b)) -> a -> Expr (Either a b)
makeError' b typeError = Expr b (Left typeError)

makeError :: Body (Expr (Either TypeError b)) -> String -> Expr (Either TypeError b)
makeError b str = makeError' b $ TypeError str


inferIndexType :: Expr a -> Expr a  -> EitherT TypeError (State Scope) InferredExpr
inferIndexType arrExpr indexExpr = do
  inferredArrExpr <- inferType arrExpr
  inferredIndexExpr <- inferType indexExpr
  let newBody = Index inferredArrExpr inferredIndexExpr
      Expr _ (JSArray elemType) = inferredArrExpr
      indexType = inferredIndexExpr
  return $ Expr newBody elemType

inferAssignType :: Expr a -> Expr a -> EitherT TypeError (State Scope) InferredExpr
inferAssignType dest src = do
  inferredDest <- inferType dest
  inferredSrc <- inferType src
  let newBody = Assign inferredDest inferredSrc
  let destType = exprData inferredDest
      srcType = exprData inferredSrc
      infer' = do
        varType <- coerceTypes destType srcType
        return $ Expr newBody varType
  case exprBody inferredDest of
    Var _ -> infer'
    Property _ _ -> infer' -- TODO update object type?
    _ -> left $ TypeError "Left-hand side of assignment is not an lvalue"

inferPropertyType :: Expr a -> String -> EitherT TypeError (State Scope) InferredExpr
inferPropertyType objExpr propName =
    do inferredObjExpr <- inferType objExpr
       let newBody = Property inferredObjExpr propName
           objType = getObjPropertyType (exprData inferredObjExpr) propName 
       case objType of
         Nothing -> left $ TypeError ("object type has no property named '" ++ propName ++ "'")
         Just propType' -> return $ Expr newBody propType'

inferCallType :: Expr a -> [Expr a] -> EitherT TypeError (State Scope) InferredExpr
inferCallType callee args = do
  inferredCallee <- inferType callee
  inferredArgs <- mapM inferType args
  callResultType <- lift allocTVar
  let newBody = Call inferredCallee inferredArgs
  let argTypes = map exprData inferredArgs
  JSFunc _ returnType' <- coerceTypes (exprData inferredCallee) (JSFunc argTypes callResultType)
  return $ Expr newBody returnType'
  
inferVarType :: String -> EitherT TypeError (State Scope) InferredExpr
inferVarType name = do
  scope <- get
  let varType = getVarType (varScope scope) name
  case varType of 
    Nothing -> left . TypeError $ "undeclared variable: " ++ name
    Just varType' -> return $ Expr (Var name) varType'

inferArrayType :: [Expr a] -> EitherT TypeError (State Scope) InferredExpr
inferArrayType exprs = 
    do inferredExprs <- forM exprs inferType
       let newBody = LitArray inferredExprs
       case inferredExprs of
         [] -> do elemType <- lift allocTVar
                  return $ Expr (LitArray inferredExprs) (JSArray elemType)
         (x:xs) -> do
           let headElemType = exprData x
           forM (map exprData inferredExprs) (coerceTypes headElemType)
           return $ Expr (LitArray inferredExprs) (JSArray headElemType)

inferFuncType :: Maybe String -> [String] -> [Statement (Expr a)] -> EitherT TypeError (State Scope) InferredExpr
inferFuncType name argNames exprs =
    do returnType' <- lift allocTVar
       -- if the function has a name, introduce it as a variable to the var context before the argument names
       funcType <- 
           case name of
             Just x -> do varScope' <- lift $ intrVars [x]
                          lift $ updateVarScope varScope'
                          return . snd . head . getVars $ varScope'
             Nothing -> lift allocTVar
       -- allocate variables for the arguments
       argScope <- lift $ intrVars argNames
       let funcScope' = FuncScope { returnType = returnType' }
           argTypes = map snd $ getVars argScope
       scope <- get
       -- infer the statements that make up this function
       let (inferredStatments, Scope typeScope'' _ updatedFuncScope) = 
                 flip runState scope { funcScope = Just funcScope', varScope = argScope } 
                          . runEitherT
                          $ forM exprs inferStatement
       -- update scope with type/unification changes from the statements' processing
       put $ scope { typeScope = typeScope'' }
       inferredStatments' <- EitherT . return $ inferredStatments
       let newBody = LitFunc name argNames inferredStatments'
       let inferredReturnType = returnType $ fromJust updatedFuncScope -- not supposed to be Nothing...
       unifiedReturnType <- coerceTypes returnType' inferredReturnType
       unifiedFuncType <- coerceTypes funcType . JSFunc argTypes $ unifiedReturnType
       return $ Expr newBody unifiedFuncType 

inferReturnType ::  Expr a -> EitherT TypeError (State Scope) InferredExpr
inferReturnType expr =
    do e'@(Expr newBody retType) <- inferType expr
       curReturnType <- getFuncReturnType
       t <- coerceTypes retType curReturnType
       return e'
 
inferObjectType :: [(String, Expr a)] -> EitherT TypeError (State Scope) InferredExpr
inferObjectType props =
    do let propNames = map fst props
       let propExprs = map snd props
       inferredProps <- mapM inferType propExprs
       let newBody = LitObject $ zip propNames inferredProps
       return . Expr newBody $ JSObject (zip propNames (map exprData inferredProps))


