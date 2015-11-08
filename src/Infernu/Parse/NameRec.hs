module Infernu.Parse.NameRec where

import Infernu.Decycle (decycle)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import Data.Functor.Identity (runIdentity)


data Names a = Names { namesNext :: a -> a, namesCurrent :: a }

data Named a x = Named a (x (Named a x))

instance Eq a => Eq (Named a x) where
    (Named a x) == (Named b y) = a == b

instance Ord a => Ord (Named a x) where
    (Named a x) `compare` (Named b y) = a `compare` b

named :: (MonadFix m, Monad m) => x (Named a x) -> StateT (Names a) m (Named a x)
named x = do
    names <- get
    let cur = namesCurrent names
        next = namesNext names cur
    put (names { namesCurrent = next })
    return (Named cur x)

type Cycle a res = Maybe (a -> res) -> a -> res

showNamed' ::
    (Show a)
    => ((Named a x -> String) -> x (Named a x) -> String)
    -> Cycle (Named a x) String
showNamed' _ Nothing _ = ""
showNamed' showX (Just r) (Named a x) = show a ++ " = (" ++ showX r x ++ ")"

showNamed ::
    (Ord a, Show a)
    => ((Named a x -> String) -> x (Named a x) -> String)
    -> Named a x -> String
showNamed f = decycle (showNamed' f)

data Expr e = Var Int | Lam Int e | App e e

showExpr _ (Var x) = "Var " ++ show x
showExpr f (Lam x e) = "\\" ++ show x ++ " -> " ++ f e
showExpr f (App e1 e2) = f e1 ++ " " ++ f e2


type NamedExpr a = Named a Expr

showNamedExpr :: (Show a, Ord a) => NamedExpr a -> String
showNamedExpr = showNamed showExpr

test :: (MonadFix m, Monad m) => StateT (Names a) m (NamedExpr a)
test = do
    x <- named $ Var 0
    id <- named $ App x x
    mfix $ \y -> named $ App x y

test2 :: NamedExpr Int
test2 = runIdentity $ evalStateT test (Names (+1) 0)

