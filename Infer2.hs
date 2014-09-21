module Infer2 where

import Data.Functor((<$>))
import Data.Functor.Identity(Identity(..), runIdentity)
import Control.Monad(forM, foldM)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State(StateT, evalStateT, runState, get, put)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, ask, local, mapReaderT)
import Control.Monad.Trans.Writer(WriterT, runWriterT, tell, listen)
import Control.Monad.Trans.Either(EitherT, runEitherT, left, right, hoistEither)
import qualified Data.Map.Lazy as Map

import Types
import Pretty

type JSTypeEnv = TypeEnv JSConsType
type JSTypeSig = TypeSig JSConsType
type JSTSubst = TSubst JSConsType
type JSTypeError = TypeError JSConsType
data NameSupply = NameSupply { maxName :: Name }

emptyNameSupply :: NameSupply
emptyNameSupply = NameSupply { maxName = 0 }

-- | An inference result with internal context.
-- | ReaderT JSTypeEnv - type signatures must be propogated only inward towards nested expressions
-- | WriterT JSTSubst - results of unification, stored in a TSubst, must be propogated outwards from nested expressions and composed with the current subst map
-- | StateT NameSupply - stateful "counter" used for naming fresh type variables
-- | EitherT TypeError - failure at any point shortcuts the result returning a Left TypeError instead.
type Infer a = ReaderT JSTypeEnv (WriterT JSTSubst (StateT NameSupply (EitherT JSTypeError Identity))) a

runInfer :: Infer a -> Either JSTypeError a
runInfer = runIdentity 
           . runEitherT 
           . flip evalStateT emptyNameSupply
           . fmap fst 
           . runWriterT 
           . flip runReaderT emptyTEnv

typeFail :: JSTypeError -> Infer a
typeFail = lift . lift . lift . left

fresh :: Infer Name
fresh = do ns <- lift . lift $ get
           let newName = 1 + maxName ns
           lift . lift . put $ ns { maxName = newName }
           return newName

askTypeEnv :: Infer JSTypeEnv
askTypeEnv = ask

withTypeEnv :: (JSTypeEnv -> JSTypeEnv) -> Infer a -> Infer a
withTypeEnv f action = local f action

listenTSubst :: Infer a -> Infer (a, JSTSubst)
listenTSubst a = mapReaderT listen $ a

tellTSubst :: JSTSubst -> Infer ()
tellTSubst = lift . tell

runEither :: Either JSTypeError r -> Infer r
runEither = lift . lift . lift . hoistEither

inferExprs :: [Expr a]  -> Infer ([JSType], JSTSubst)
inferExprs [] = return ([], idSubst)
inferExprs (e:es) = do
  (t, s) <- listenTSubst $ inferExpr e
  foldM inferExprs' ([t], s) es

inferExprs' :: ([JSType], JSTSubst) -> Expr a -> Infer ([JSType], JSTSubst)
inferExprs' (ts, lastSub) argExpr = do
  (argType, newSub) <- listenTSubst $ withTypeEnv (substituteTE lastSub) (inferExpr argExpr)
  return (argType : ts, newSub `compose` lastSub) 

inferExpr :: Expr a -> Infer JSType
inferExpr (Expr body _) =
    case body of
      -- todo other builtin types
      --LitFunc name argNames stmts -> inferFunc name argNames stmts
      Assign lval rval -> inferAssign lval rval
      Call callee args -> inferCall callee args
      Index expr indexExpr -> inferIndex expr indexExpr
      LitArray exprs -> inferArray exprs
      LitBoolean _ -> return JSBoolean
      LitNumber _ -> return JSNumber
      LitRegex _ -> return JSRegex
      LitString _ -> return JSString
      Property expr propName -> inferProperty expr propName
      Var name -> inferVar name

inferProperty :: Expr a -> String -> Infer JSType
inferProperty expr propName = do
  propTypeName <- fresh
  let propType = JSTVar propTypeName
  (objType, subst) <- listenTSubst $ inferExpr expr
  subst' <- runEither $ unify subst (toType objType) (toType $ JSObject [(propName, propType)])
  let finalSubst = subst' `compose` subst
  tellTSubst finalSubst
  return $ fromType . substituteType finalSubst . toType $ propType

inferIndex :: Expr a -> Expr a -> Infer JSType
inferIndex expr indexExpr = do
  elemTypeName <- fresh
  let elemType = JSTVar elemTypeName
  (rt, rs) <- listenTSubst $ inferExpr expr
  s1 <- runEither $ unify rs (toType rt) (toType $ JSArray elemType)
  (lt, ls) <- listenTSubst $ withTypeEnv (substituteTE s1) $ inferExpr indexExpr
  s2 <- runEither $ unify ls (toType lt) (toType JSNumber)
  let finalSubst = s2 `compose` ls `compose` s1 `compose` rs
  tellTSubst finalSubst
  return $ fromType . substituteType finalSubst . toType $ elemType

inferAssign :: Expr a -> Expr a -> Infer JSType
inferAssign lval rval = do
  (rt, rs) <- listenTSubst $ inferExpr lval
  (lt, ls) <- listenTSubst $ withTypeEnv (substituteTE rs) $ inferExpr rval
  subst' <- runEither $ unify ls (toType rt) (toType lt)
  let finalSubst = subst' `compose` ls `compose` rs
  tellTSubst finalSubst
  return $ fromType . substituteType finalSubst . toType $ rt


-- page 178
-- | instantiates a given type signature: allocates fresh variables for all the bound names and replaces them in the type
newInstance :: JSTypeSig -> Infer JSType
newInstance (TypeSig varNames t) = 
    do substList <- forM varNames $ \name -> 
                do tname <- fresh
                   return (name, TVar tname)
       return . fromType $ substituteType (substFromList substList) t

-- | Infers a value variable expression. If the variable is assigned a type signature, instantiate it. Otherwise fail.
inferVar :: String -> Infer JSType
inferVar name = do
  tenv <- askTypeEnv
  case Map.lookup name tenv of
    Just tsig -> newInstance tsig
    Nothing -> typeFail $ GenericTypeError ("Unbound variable: " ++ name)
  
inferCall :: Expr a -> [Expr a] -> Infer JSType
inferCall callee args = do
  returnTName <- fresh
  let returnTVar = JSTVar returnTName
  (calleeType:argTypes, substN) <- inferExprs $ callee:args
  newTSubst <- runEither $ unify substN (toType $ JSFunc argTypes returnTVar) (substituteType substN $ toType calleeType) 
  let finalSubst = newTSubst `compose` substN
  tellTSubst finalSubst
  return . fromType $ substituteType finalSubst (toType returnTVar)


inferFunc :: Maybe String -> [String] -> [Statement a] -> Infer JSType
inferFunc name argName stmts = do
  funcTypeName <- fresh
  tenv <- askTypeEnv  
  let tenv' = case name of
                Nothing -> tenv -- anonymous function - doesn't introduce a new local name
                Just name' -> setTypeSig name' (TypeSig [] (TVar funcTypeName)) tenv
  _ <- withTypeEnv (const tenv') $ forM stmts inferStatement
  return $ JSFunc  

unifyExprs' ::  JSType -> JSTSubst -> JSType -> Infer JSTSubst
unifyExprs' elemType lastSubst curType = runEither $ unify lastSubst (toType elemType) (toType curType)

inferArray :: [Expr a] -> Infer JSType
inferArray exprs = do
  elemTVarName <- fresh
  let elemType = JSTVar elemTVarName
  (types, subst) <- inferExprs exprs
  finalSubst <- foldM (unifyExprs' elemType) subst types
  tellTSubst finalSubst
  return $ JSArray (fromType . substituteType finalSubst $ toType elemType)


inferStatement :: Statement (Expr a) -> Infer ()
inferStatement Empty = return ()
inferStatement (Expression expr) = inferExpr expr >> return ()

----------------

ex b = Expr b ()

arrayTest = ex $ Index (ex $ LitArray [ex $ LitBoolean False, ex $ LitBoolean True]) (ex $ LitNumber 32)

infArrayTest = runInfer $ inferExpr arrayTest
