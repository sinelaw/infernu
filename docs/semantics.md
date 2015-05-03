# Semantics of JS according to Infernu

These definitions don't necessarily correspond to ECMAScript.

> s[[ ]] :: Statement -> Denotation
> e[[ ]] :: Expression -> Denotation

### RealWorld

> store :: Location -> Value -> RealWorld -> RealWorld
> load :: Location -> RealWorld -> Value
  
### Environment

> type Env = [(Name, Value, Location)]

> pushDecl x v ((_,_,l):env) = (x,v,l+1):env
> popDecl (p:env) = env
> composeEnv env1 env2 = env1 ++ env2
> -- TODO: deal with failure
> get      x env = case lookup x env of (_,v,_) -> v
> location x env = case lookup x env of (_,_,l) -> l

### Statements

> s[[ stmt ]] :: (Env -> RealWorld -> (Env, RealWorld)) -> Env -> RealWorld -> (Env, RealWorld)

### Expressions

> e[[ expr ]] :: Env -> RealWorld -> (RealWorld, Value)

### Expression statements

> s[[ expr ]] = \k env rw -> k env . fst . e[[ expr ]] env $ rw

### Statement sequence ;

> s[[ stmtA; stmtB ]] = s[[ stmtA ]] . s[[ stmtB ]]

### Variable declaration

TODO

> s[[ var x ]] = \k env rw -> k (pushDecl id[[ x ]] env) rw

### Assignment

> s[[ x = expr ]] = \k env rw ->
>     case (e[[ expr ]] env rw) of
>         (rw', val) -> k env (store (get id[[ x ]] env) val rw')

## While loop

> s[[ while (expr) { body } ]] =
>     \k env rw ->
>         let w rw' = case e[[ expr ]] env rw' of
>                         (rw'', False) -> k env rw''
>                         (rw'', True)  -> w rw''
>         in w rw

## Case (statements/expressions?)

TODO

## Function expressions

> e[[ function(args) { body } ]] =
>     \env rw ->
>         ( rw
>         , \k rw' ->
>               \this args ->
>                   [[ body ]] k (composeEnv args (pushDecl "this" this env)) rw'

## Non-JS fragments

These syntax constructs are added:

### Let expression

Non-recursive ('x' not free in 'expr'):

> s[[ let x = expr in stmt ]] = \k env rw ->
>     case e[[ expr ]] env rw of
>         (rw', v) -> [[ stmt ]] k (pushDecl id[[ x ]] v env) rw'

Recursive:

> s[[ let x = expr{x} in stmt ]] = s[[ let x = fix(\x -> expr{x}) in stmt ]]

Where the notation 'expr{x}' means "an expression that has 'x' as a free variable'.

Note that this definition restricts to non-polymorphic recursion.


### Fix

TODO
 
> e[[ fix ]] = \env -> \rw ->
