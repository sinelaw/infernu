Semantics of JS according to Infernu
====================================

These definitions don't necessarily correspond to ECMAScript.

``` sourceCode
s[[ ]] :: Statement -> Denotation
e[[ ]] :: Expression -> Denotation
```

### RealWorld

``` sourceCode
store :: Location -> Value -> RealWorld -> RealWorld
load :: Location -> RealWorld -> Value
```

### Environment

``` sourceCode
type Env = [(Name, Value, Location)]
```

``` sourceCode
pushDecl x v (p@(_,_,l):env) = (x,v,l+1):p:env
popDecl (p:env) = env
composeEnv env1 env2 = env1 ++ env2
-- TODO: deal with failure
get      x env = case lookup x env of (_,v,_) -> v
location x env = case lookup x env of (_,_,l) -> l
```

### Statements

``` sourceCode
s[[ stmt ]] :: (Env -> RealWorld -> (Env, RealWorld))
               -> Env -> RealWorld -> (Env, RealWorld)
```

``` sourceCode
halt = (,)
```

### Expressions

``` sourceCode
e[[ expr ]] :: Env -> RealWorld -> (RealWorld, Value)
```

### Function expressions

Return values are passed using a special "return" name pushed onto the environment. The result of a function is the value bound in the environment to that value when the function completes.

``` sourceCode
e[[ function(args) { body } ]] =
    \env rw ->
        ( rw
        , \rw' ->
              \this args ->
                  case [[ body ]] halt (composeEnv args (pushDecl "this" this $ pushDecl "return" e[[ undefined ]] env)) rw' of
                      (env'', rw'') -> (rw'', get "return" env'')
        )
```

### Function Call

``` sourceCode
e[[ f(args) ]] =
    \env rw ->
        case e[[ f ]] env rw of
            (rw', f') -> f' rw' e[[ undefined ]] e[[ args ]]
```

### Return statement

``` sourceCode
s[[ return expr; stmt ]] =
    \k env rw -> case [[ expr ]] env rw of
                     (rw', val) -> k (pushDecl "return" val env) rw'
```

### Expression statements

``` sourceCode
s[[ expr ]] = \k env rw -> k env . fst . e[[ expr ]] env $ rw
```

### Statement sequence ;

``` sourceCode
s[[ stmtA; stmtB ]] = s[[ stmtA ]] . s[[ stmtB ]]
```

### (Mutable) variable declaration

``` sourceCode
s[[ var x ]] = \k env rw -> k (pushDecl id[[ x ]] env) rw
```

### Assignment

``` sourceCode
s[[ x = expr ]] = \k env rw ->
    case (e[[ expr ]] env rw) of
        (rw', val) -> k env (store (get id[[ x ]] env) val rw')
```

### While loop

``` sourceCode
s[[ while (expr) { body } ]] =
    \k env rw ->
        let w rw' = case e[[ expr ]] env rw' of
                        (rw'', False) -> k env rw''
                        (rw'', True)  -> w rw''
        in w rw
```

Note: Recursive let using in the meaning here. It should be the same as using `fix`.

### If statement

``` sourceCode
s[[ if expr then b1 else b2 ]] =
    \k env rw ->
        case e[[ expr ]] env rw of
            (rw', True)  -> [[ b1 ]] k env rw'
            (rw', False) -> [[ b2 ]] k env rw'
```

Non-JS fragments
----------------

These syntax constructs are added:

### Let expression (immutable variable)

Non-recursive ('x' not free in 'expr'):

``` sourceCode
s[[ let x = expr in stmt ]] = \k env rw ->
    case e[[ expr ]] env rw of
        (rw', v) -> [[ stmt ]] k (pushDecl id[[ x ]] v env) rw'
```

Recursive:

``` sourceCode
s[[ let x = expr in stmt ]] = s[[ let x = fix(\x -> expr) in stmt ]]
```

Where `x` is free in `expr`.

Note that this definition restricts to non-polymorphic recursion.

### Fix

TODO

``` sourceCode
e[[ fix ]] = \env -> \rw ->
```
