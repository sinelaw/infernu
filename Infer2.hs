module Infer2 where

import Data.Functor((<$>))
import Data.Functor.Identity(Identity(..))
import Control.Monad(forM)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State(StateT, runState, get, put)
import Control.Monad.Trans.Reader(ReaderT, ask, local)
import Control.Monad.Trans.Writer(WriterT, tell, listen)
import Control.Monad.Trans.Either(EitherT, left, right)
import qualified Data.Map.Lazy as Map

import Types

type JSTypeEnv = TypeEnv JSConsType
type JSTypeSig = TypeSig JSConsType
type JSTSubst = TSubst JSConsType

data NameSupply = NameSupply { maxName :: Name }

type Infer a = StateT NameSupply (ReaderT JSTypeEnv (WriterT JSTSubst (EitherT TypeError Identity))) a

typeFail :: TypeError -> Infer a
typeFail = lift . lift . lift . left

fresh :: Infer Name
fresh = do ns <- get
           let newName = 1 + maxName ns
           put $ ns { maxName = newName }
           return newName

inferExpr :: Expr a -> Infer JSType
inferExpr (Expr body _) =
    case body of
      LitBoolean _ -> return JSBoolean
      -- todo other builtin types
      Var name -> inferVar name
      Call callee args -> inferCall callee args


-- page 178
newInstance :: JSTypeSig -> Infer JSType
newInstance (TypeSig varNames t) = 
    do substList <- forM varNames $ \name -> 
                do tname <- fresh
                   return (name, TVar tname)
       return . fromType $ substituteType (substFromList substList) t

inferVar :: String -> Infer JSType
inferVar name = do
  tenv <- lift ask
  case Map.lookup name tenv of
    Just tsig -> newInstance tsig
    Nothing -> typeFail $ TypeError ("Unbound variable: " ++ name)
  

inferCall :: Expr a -> [Expr a] -> Infer JSType
inferCall callee args = do
  tenv <- lift ask
  returnTName <- fresh
--  (t1, s1) <- lift . lift . listen $ inferExpr callee
--  (t2, s2) <- getSubst $ inferExpr args
  return JSBoolean

  
