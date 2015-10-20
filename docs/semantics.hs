-- | Semantics of JS according to Infernu
-- These definitions don't necessarily correspond to ECMAScript.

module Semantics where

import Data.Map (Map)
import qualified Data.Map as Map

-- | The term language

data Id = NameId String | ReturnId deriving (Show, Eq, Ord)

data Value = VString String
           | VBool Bool
           | VNum Double
           | VUndef
           | VRow (Map String Value)
           | VFun (RealWorld -> Value -> Value -> (RealWorld, Value))

instance Show Value where
    show (VString s) = "VString " ++ show s
    show (VBool b) = "VBool " ++ show b
    show (VNum b) = "VNum " ++ show b
    show VUndef = "undefined"
    show (VRow props) = "VRow " ++ show props
    show (VFun _) = "VFun" -- "VFun (" ++ show props ++ ", <fun>)"

data Expr = Var String
          | Lit Value
          | App Expr Expr
          | Abs String Statement

data Statement = Empty
               | Seq Statement Statement -- bug (continuation always here or in all other stmts?)
               | Return Expr Statement
               | JustExpr Expr
               | VarDecl String
               | Assign String Expr
               | While Expr Statement
               | If Expr Statement Statement
               | Let String Expr Statement

-- | RealWorld

newtype Location = Location Int deriving (Ord, Eq, Show)
type RealWorld = Map Location Value

initialWorld = Map.empty

store :: Location -> Value -> RealWorld -> RealWorld
store = Map.insert

load :: Location -> RealWorld -> Value
load l rw = case Map.lookup l rw of
             Just v -> v
             Nothing -> error $ "Unallocated location: " ++ show l

-- | Environment

newtype Env = Env [(Id, (Value, Location))]

emptyEnv :: Env
emptyEnv = Env []

push :: Id -> Value -> Env -> Env
push x v (Env []) = Env [(x,(v,Location 0))]
push x v (Env (p@(_,(_,Location l)):env)) = Env $ (x,(v,Location $ l+1)):p:env

pushs :: Env -> Env -> Env
pushs (Env []) env = env
pushs (Env ((x,(v, _)):xs)) env = pushs (Env xs) $ push x v env

composeEnv :: Env -> Env -> Env
composeEnv (Env env1) (Env env2) = Env $ env1 ++ env2


lookup' :: Id -> Env -> (Value, Location)
lookup' x (Env env) = case lookup x env of
    Just p -> p
    Nothing -> error $ "Undeclared name: " ++ show x

get :: Id -> Env -> Value
get      x env = fst $ lookup' x env

location :: Id -> Env -> Location
location x env = snd $ lookup' x env

-- | Expressions

emean :: Expr -> Env -> RealWorld -> (RealWorld, Value)

emean (Lit v) = \_env rw -> (rw, v)

emean (Var x) = \env rw -> (rw, get (NameId x) env)

-- | Function expressions

--Return values are passed using a special "return" name pushed onto the environment. The result of a function is the value bound in the environment to that value when the function completes.

emean (Abs args body) =
    \env rw ->
        ( rw
        , VFun
          $ \rw' ->
              \this args' ->
                  let bodyEnv =
                          push (NameId args) args'
                          . push (NameId "this") this
                          . push ReturnId VUndef
                          $ env

                  in case (smean body) halt bodyEnv rw' of
                        (env'', rw'') -> (rw'', get ReturnId env'')
        )

-- | Function Call

emean (App f args) =
    \env rw ->
        case emean f env rw of
            (rw', VFun f') -> case (emean args env rw') of (rw'', v) -> f' rw'' VUndef v
            _ -> error "Expected a function"



-- | Statements

halt :: Env -> RealWorld -> (Env, RealWorld)
halt = (,)

smean :: Statement
         -> (Env -> RealWorld -> (Env, RealWorld))
         -> Env -> RealWorld -> (Env, RealWorld)

smean Empty = id

-- | Return statement

smean (Return expr _stmt) = -- continuation stmt is ignored
    \k env rw -> case (emean expr) env rw of
                     (rw', val) -> k (push ReturnId val env) rw'

-- | Expression statements

smean (JustExpr expr) = \k env rw -> k env . fst . (emean expr) env $ rw

-- | Statement sequence ;

smean (Seq stmtA stmtB) = smean stmtA . smean stmtB

-- | (Mutable) variable declaration

smean (VarDecl x) = \k env rw -> k (push (NameId x) VUndef env) rw

-- | Assignment

smean (Assign x expr) = \k env rw ->
    case ((emean expr) env rw) of
        (rw', val) -> k env (store (location (NameId x) env) val rw')

-- | While loop

smean (While expr stmt) =
    \k env rw ->
        let w rw' = case (emean expr) env rw' of
                        (rw'', VBool False) -> k env rw''
                        (rw'', VBool True)  -> w rw''
                        _ -> error "Expected boolean"
        in w rw

--Note: Recursive let using in the meaning here. It should be the same as using `fix`.

-- | If statement

smean (If expr b1 b2) =
    \k env rw ->
        case (emean expr) env rw of
            (rw', VBool True)  -> (smean b1) k env rw'
            (rw', VBool False) -> (smean b2) k env rw'
            _ -> error "Expected boolean"



-- | Non-JS fragments

--These syntax constructs are added:

-- | Let expression (immutable variable)

--Non-recursive ('x' not free in 'expr'):

smean (Let x expr stmt) = \k env rw ->
    case (emean expr) env rw of
        (rw', v) -> (smean stmt) k (push (NameId x) v env) rw'

--Recursive:

-- s[[ let x = expr in stmt ]] = s[[ let x = fix(\x -> expr) in stmt ]]

--Where `x` is free in `expr`.

--Note that this definition restricts to non-polymorphic recursion.



-- | Fix

--TODO

-- e[[ fix ]] = \env -> \rw ->


test :: Bool -> Expr
test f = (App (Abs "y"
               (Seq
                (Return (Lit $ VNum 1) Empty)
                (If (Var "y")
                 (Return (Lit $ VNum 2) Empty)
                 (Return (Lit $ VNum 3) Empty)
                ))
              )
          (Lit $ VBool f))

