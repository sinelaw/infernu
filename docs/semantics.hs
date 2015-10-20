-- | Semantics of JS according to Infernu
-- These definitions don't necessarily correspond to ECMAScript.

module Semantics where

import Data.Map (Map)
import qualified Data.Map as Map

-- | The term language

newtype Id = Id String
           deriving (Show, Eq, Ord)

data Value = VString String
           | VBool Bool
           | VNum Double
           | VUndef
           | VRecord (Map String Location)
           | VFun (RealWorld -> Value -> Value -> (RealWorld, Value))

instance Show Value where
    show (VString s) = "VString " ++ show s
    show (VBool b) = "VBool " ++ show b
    show (VNum b) = "VNum " ++ show b
    show VUndef = "undefined"
    show (VRecord props) = "VRecord " ++ show props
    show (VFun _) = "VFun" -- "VFun (" ++ show props ++ ", <fun>)"

data Operator = Plus | Minus | GreaterThan
              deriving (Show)

data Expr = Var String
          | Lit Value
          | App Expr Expr
          | Abs String Statement
          | Op Operator Expr Expr
          deriving (Show)

data Statement = Empty
               | Seq Statement Statement
               | Return Expr
               | JustExpr Expr
               | VarDecl String
               | Assign String Expr
               | While Expr Statement
               | If Expr Statement Statement
               -- Let String Expr Statement
               deriving (Show)

-- | RealWorld

newtype Location = Location Int deriving (Ord, Eq, Show)
data RealWorld = RealWorld (Map Location Value) Location
                 deriving (Show)

initialWorld :: RealWorld
initialWorld = RealWorld Map.empty (Location 0)

alloc :: RealWorld -> (RealWorld, Location)
alloc (RealWorld m (Location l)) = ( RealWorld m (Location (l + 1))
                                   , Location l)

store :: Location -> Value -> RealWorld -> RealWorld
store l v (RealWorld m maxLoc) = RealWorld (Map.insert l v m) maxLoc

load :: RealWorld -> Location -> Value
load (RealWorld m _) l = case Map.lookup l m of
    Just v -> v
    Nothing -> error $ "Unallocated location: " ++ show l

-- | Environment

data Env = Env { bindings :: [(Id, Location)]
               , returns :: [Value] }
         deriving (Show)

emptyEnv :: Env
emptyEnv = Env [] []

push :: Id -> Location -> Env -> Env
push x l (Env bs rs) = Env ((x, l) : bs) rs

pushReturn :: Value -> Env -> Env
pushReturn r (Env bs rs) = Env bs (r:rs)

popReturn :: Env -> (Env, Value)
popReturn (Env bs []) = error "No more returns"
popReturn (Env bs (r:rs)) = (Env bs rs, r)

lookup' :: Id -> Env -> Location
lookup' x (Env bs _) = case lookup x bs of
    Just p -> p
    Nothing -> error $ "Undeclared name: " ++ show x ++ " not in env: " ++ show bs

get :: Id -> Env -> RealWorld -> Value
get x env rw = load rw $ lookup' x env

location :: Id -> Env -> Location
location = lookup'

-- | Expressions

emean :: Expr -> Env -> RealWorld -> (RealWorld, Value)

emean (Lit v) = \_env rw -> (rw, v)

emean (Var x) = \env rw -> (rw, get (Id x) env rw)

emean (Op op x y) = \env rw ->
    case emean x env rw of
    (rw', VNum vx) -> case emean y env rw' of
        (rw'', VNum vy) -> (rw'', vx `f` vy)
    where f = case op of
              Plus -> \a b -> VNum (a + b)
              Minus -> \a b -> VNum (a - b)
              GreaterThan -> \a b -> VBool (a > b)

-- | Function expressions
--
-- The meaning of Abs is a function that takes values and returns a value.
--
-- Because the function takes values, not locations, and because there
-- is no "pointer" in the language, calls are by value. The record
-- value is special because it's a map from names to locations, so you
-- can mutably modify the data.
--
-- Return values are passed using a special "return" name pushed onto
-- the environment. The result of a function is the value bound in the
-- environment to that value when the function completes.

emean (Abs args body) =
    \env rw ->
        ( rw
        , VFun
          $ \rw1 ->
              \this args' ->
                  let (rw2, argLoc) = alloc rw1
                      (rw3, thisLoc) = alloc rw2
                      rw4 = store argLoc args' rw3
                      rw5 = store thisLoc this rw4
                      bodyEnv =
                          push (Id args) argLoc
                          . push (Id "this") thisLoc
                          $ env

                  in case (smean body) halt bodyEnv rw5 of
                        (env'', rw6) -> (rw6, snd $ popReturn env'')
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

smean (Return expr) =
    \k env rw -> case (emean expr) env rw of
                     (rw', val) -> halt (pushReturn val env) rw'

-- | Expression statements

smean (JustExpr expr) = \k env rw -> k env . fst . (emean expr) env $ rw

-- | Statement sequence ;

smean (Seq stmtA stmtB) = \k env rw ->
    smean stmtA (smean stmtB k) env rw

-- | (Mutable) variable declaration

smean (VarDecl x) = \k env rw ->
    case alloc rw of
    (rw', loc) -> k (push (Id x) loc env) rw'

-- | Assignment

smean (Assign x expr) = \k env rw ->
    case ((emean expr) env rw) of
        (rw', val) -> k env (store (location (Id x) env) val rw')

-- | While loop

smean (While expr body) = while
    where
        while =
            \k env rw ->
                case (emean expr) env rw of
                (rw', VBool False) -> k env rw'
                (rw', VBool True)  -> smean body (while k) env rw'
                _ -> error "Expected boolean"

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

-- smean (Let x expr stmt) = \k env rw ->
--     case (emean expr) env rw of
--         (rw', v) -> (smean stmt) k (push (Id x) v env) rw'

--Recursive:

-- s[[ let x = expr in stmt ]] = s[[ let x = fix(\x -> expr) in stmt ]]

--Where `x` is free in `expr`.

--Note that this definition restricts to non-polymorphic recursion.



-- | Fix

--TODO

-- e[[ fix ]] = \env -> \rw ->
block = foldr Seq Empty

-- | test
-- >>> snd $ emean (testReturn True) emptyEnv initialWorld
-- VNum 1.0
testReturn :: Bool -> Expr
testReturn f = (App (Abs "y"
               (Seq
                (Return (Lit $ VNum 1) )
                (If (Var "y")
                 (Return (Lit $ VNum 2))
                 (Return (Lit $ VNum 3))
                ))
              )
          (Lit $ VBool f))

-- | test
-- >>> snd $ emean (testWhile 3) emptyEnv initialWorld
-- VNum 6.0
testWhile :: Double -> Expr
testWhile x = App (Abs "x"
                   $ block [ sumUpToXIntoY
                           , Return (Var "y")
                           ]
                  ) (Lit $ VNum x)
    where
        sumUpToXIntoY =
            block
            [ VarDecl "y"
            , Assign "y" (Lit (VNum 0))
            , While
              (Op GreaterThan (Var "x") (Lit (VNum 0)))
              $ block
              [ Assign "y" (Op Plus (Var "y") (Var "x"))
              , Assign "x" (Op Minus (Var "x") (Lit (VNum 1)))
              ]
            ]

-- | test
-- >>> snd $ emean (test True) emptyEnv initialWorld
-- VNum 2.0
test :: Bool -> Expr
test f = (App (Abs "y"
                (If (Var "y")
                 (Return (Lit $ VNum 2))
                 (Return (Lit $ VNum 3))
                )
              )
          (Lit $ VBool f))

