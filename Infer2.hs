module Infer2 where

import Data.Functor((<$>))
import Data.Functor.Identity(Identity(..))
import Control.Monad(forM, foldM)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State(StateT, runState, get, put)
import Control.Monad.Trans.Reader(ReaderT, ask, local, mapReaderT)
import Control.Monad.Trans.Writer(WriterT, tell, listen)
import Control.Monad.Trans.Either(EitherT, left, right, hoistEither)
import qualified Data.Map.Lazy as Map

import Types

type JSTypeEnv = TypeEnv JSConsType
type JSTypeSig = TypeSig JSConsType
type JSTSubst = TSubst JSConsType

data NameSupply = NameSupply { maxName :: Name }

-- | An inference result with internal context.
-- | ReaderT JSTypeEnv - type signatures must be propogated only inward towards nested expressions
-- | WriterT JSTSubst - results of unification, stored in a TSubst, must be propogated outwards from nested expressions and composed with the current subst map
-- | StateT NameSupply - stateful "counter" used for naming fresh type variables
-- | EitherT TypeError - failure at any point shortcuts the result returning a Left TypeError instead.
type Infer a = ReaderT JSTypeEnv (WriterT JSTSubst (StateT NameSupply (EitherT TypeError Identity))) a

typeFail :: TypeError -> Infer a
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

runEither :: Either TypeError r -> Infer r
runEither = lift . lift . lift . hoistEither

inferExpr :: Expr a -> Infer JSType
inferExpr (Expr body _) =
    case body of
      LitBoolean _ -> return JSBoolean
      -- todo other builtin types
      Var name -> inferVar name
      Call callee args -> inferCall callee args
      LitFunc name argNames stmts -> inferFunc name argNames stmts

inferExprs :: [Expr a]  -> Infer ([JSType], JSTSubst)
inferExprs [] = return ([], idSubst)
inferExprs (e:es) = do
  (t, s) <- listenTSubst $ inferExpr e
  foldM inferExprs' ([t], s) es

inferExprs' :: ([JSType], JSTSubst) -> Expr a -> Infer ([JSType], JSTSubst)
inferExprs' (ts, lastSub) argExpr = do
  (argType, newSub) <- listenTSubst $ withTypeEnv (substituteTE lastSub) (inferExpr argExpr)
  return (argType : ts, newSub `compose` lastSub) 


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
    Nothing -> typeFail $ TypeError ("Unbound variable: " ++ name)
  
inferCall :: Expr a -> [Expr a] -> Infer JSType
inferCall callee args = do
  returnTName <- fresh
  let returnTVar = JSTVar returnTName
  (calleeType, subst1) <- listenTSubst $ inferExpr callee
  (argTypes, substN) <- inferExprs args
  newTSubst <- runEither $ unify substN (substituteType substN $ toType calleeType) (toType $ JSFunc argTypes returnTVar)
  tellTSubst $ newTSubst `compose` substN
  return . fromType $ substituteType newTSubst (toType returnTVar)


inferFunc :: Maybe String -> [String] -> [Statement a] -> Infer JSType
inferFunc name argName stmts = do
  funcTypeName <- fresh
  tenv <- askTypeEnv  
  let tenv' = case name of
                Nothing -> tenv
                Just name' -> setTypeSig name' (TypeSig [] (TVar funcTypeName))
  (funcType, subst) <- inferExpr tenv' ....stmts....
