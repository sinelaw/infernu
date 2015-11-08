module Infernu.Parse.NameRec where

import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Trans.State (StateT, get, put, evalStateT)

import Infernu.Decycle (decycle)

import Data.Monoid ((<>))

data Names a = Names { namesNext :: a -> a, namesCurrent :: a }

-- | A bird has whispered in my ear that Named is actually Cofree from
-- Control.Comonad.Cofree, except that we don't require x to be a functor.
data Named a x = Named { name :: a, nameUnwrap :: x (Named a x) }

-- | Equality is by name only
instance Eq a => Eq (Named a x) where
    (Named a _) == (Named b _) = a == b

-- | Ord is by name only
instance Ord a => Ord (Named a x) where
    (Named a _) `compare` (Named b _) = a `compare` b

named :: (MonadFix m, Monad m) => x (Named a x) -> StateT (Names a) m (Named a x)
named x = do
    names <- get
    let cur = namesCurrent names
        next = namesNext names cur
    put (names { namesCurrent = next })
    return (Named cur x)

-- | Utilities:

type Cycle a res = Maybe (a -> res) -> a -> res

convertNamed' :: Monoid b
    => (a -> b)
    -> ((Named a x -> b) -> x (Named a x) -> b)
    -> Cycle (Named a x) b
convertNamed' convertA _ Nothing (Named a _) = convertA a
convertNamed' convertA convertX (Just r) (Named a x) = convertA a <> convertX r x

convertNamed ::
    (Ord a, Monoid b)
    => (a -> b)
    -> ((Named a x -> b) -> x (Named a x) -> b)
    -> Named a x -> b
convertNamed fa fx = decycle (convertNamed' fa fx)

showNamed ::
    (Ord a, Show a)
    => ((Named a x -> String) -> x (Named a x) -> String)
    -> Named a x -> String
showNamed fx = convertNamed (\a -> "@" ++ show a) (\r x -> "(" ++ fx r x ++ ")")


runNamed :: Monad m => StateT s m a -> s -> m a
runNamed x names = evalStateT x names

----------------------------------------------------------------------

-- Test example:

-- data Expr e = Var Int | Lam Int e | App e e
--             deriving (Functor, Foldable, Traversable)

-- showExpr :: (a -> String) -> Expr a -> String
-- showExpr _ (Var x) = "Var " ++ show x
-- showExpr f (Lam x e) = "\\" ++ show x ++ " -> " ++ f e
-- showExpr f (App e1 e2) = f e1 ++ " " ++ f e2


-- type NamedExpr a = Named a Expr

-- showNamedExpr :: (Show a, Ord a) => NamedExpr a -> String
-- showNamedExpr = showNamed showExpr

-- test :: (MonadFix m, Monad m) => StateT (Names a) m (NamedExpr a)
-- test = do
--     x <- named $ Var 0
--     mfix $ \y -> named $ App x y

-- test2 :: NamedExpr Int
-- test2 = runIdentity $ runNamed test (Names (+1) 0)
