module Test2 where

data Expr = Plus Expr Expr | Times Expr Expr | Abs Expr | SigNum Expr | FromInteger Integer | Minus Expr Expr | Val Integer
  deriving (Show, Eq)

instance Num Expr where
  --a + (Minus (Val 0) b) = Minus a b
  --(Minus (Val 0) b) + a = Minus a b
  a + b = Plus a b
  --a * (Val 0) = (Val 0)
  --(Val 0) * a = (Val 0)
  --a * (Val 1) = a
  --(Val 1) * a = a
  a * b = Times a b
  abs = Abs
  signum = SigNum
  fromInteger = FromInteger
  negate = Minus (Val 0)

instance Ord Expr where
  e1 <= e2 = eval e1 <= eval e2

binPretty binop e1 e2 = pretty e1 ++ binop ++ pretty e2

pretty expr = case expr of
  Plus x y -> binPretty "+" x y
  Minus x y -> binPretty "-" x y
  Times x y -> "(" ++ pretty x ++ ") * (" ++ pretty y ++ ")"
  Abs expr -> "abs(" ++ pretty expr ++ ")"
  SigNum expr -> "signum(" ++ pretty expr ++ ")"
  FromInteger i -> show i  --"signum(" ++ pretty expr ++ ")"
  Val i -> show i

binEval g x y = eval x `g` eval y

eval expr = case expr of
  Plus e1 e2 -> binEval (+) e1 e2
  Times e1 e2 -> binEval (*) e1 e2
  Abs e -> abs $ eval e
  SigNum e -> signum $ eval e
  FromInteger e -> fromInteger e
  Minus e1 e2 -> binEval (-) e1 e2
  Val i -> i

fac n = if n < 2 then 1 else n * (fac (n - 1))
