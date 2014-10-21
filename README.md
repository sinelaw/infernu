# sjs

Safe JavaScript

## Two approaches

There are two general approaches to implement type inference on a new syntax:

1. Implement type inference directly on the given AST
2. Map the AST to a simpler core language, and use well-understood algorithms to infer types on the core language. Finally, map the inferred types back to the original AST.

Currently the (incomplete, buggy) code takes the first approach - it infers types directly on (a subset of) the JS syntax. However to make the code simpler and more robust, it may make more sense to take the translation approach.

One advantage of the direct approach (no translation) is that it allows us to directly deal with language constructs such as while, for, if, return, and the quirks that come with them.

### Mapping to a core language

One possible target core language is Damas-Hindley-Milner (simply typed lambda calculus with parameteric polymorphism and let-bindings) plus row type polymorphism, reference cells, and something similar to SML-style value restriction.

The mapping will have to deal with statement sequences (function and if/while/for blocks), various statement types, etc. An example mapping is:

- Statment sequences such as `st1; st2;` to `(\_ -> st2) st1`
- `function f(a,b) { statements... }` to `\a -> \b -> ...`
- `var x = 2; x = x + 1` to `let x = ref 2 in x := !x + 1`
- and so on.


## Polymorphism and the value restriction

Javascript variables are storage cells. If we compare JS to Standard ML, JS variables are equivalent to `ref a` values. JS variables are *not* names bound by `let` expressions. SML has the concept of value restriction: but it applies to let-bound names. Reference cells in SML are always monotypes. If we followed suit when inferring JS variable types, all our variables would be monotypes and we would lose all polymorphism. Consider the following examples:

    var x = 1;
    x = 'a'; // should not type check

Here we have assigned two types to the same variable (recall that variables are reference cells), which should not be allowed.

    var x = [];
    x = [1];
    x = ['a']; // should not type check

Here again we used two types. Unlike the previous example, we initially assign a value with unknown type (`[a]`, but we don't know what `a` is).

    var x;
    x = 1;
    x = 'a'; // should not type check

In the above example we have no information at all about `x`'s type at declaration time. There is no analogy for this in SML-like languages.

    var f = function (x) { return x; }
    var n = f(1);
    var s = f('a'); // ok

Here we have assigned a polymorphic function (of type `a -> a`) to the variable `f`. Later we invoke this function twice, using different types each time, which should be allowed. This is an example of desired polymorphism.

    var f = function() { return function(x) { return x; } }
    var g = f(); // g should also be polymorphic
    var n = g(1);
    var s = g('a'); // this is ok, but value restriction would not allow this to type check

Here again we make use of polymorphism. However, because we're assigning `g` from an expression that isn't a syntactic value (`f()` is a function invocation), languages such as SML will restrict `g` to a monotype and unify `g` to type `number -> number` after the call to `g(1)`. When designing our type system we must consider applying this limitation, to avoid other problems that the value restriction was designed to avoid.

    var x = 1;
    var f = function(y) { var res = x; x = y; return res; }
    var t1 = f('a');
    var t2 = f(1);

The above example is a typical pathological case that the value restriction was designed to avoid. The second call to `f` will return a string.

A variable's type can't be determined at declaration time (`var x;`). Only when the variable is assigned `x = expr` we can infer its type. The declaration serves simply to bind the variable's name to the current scope and to possibly shadow variables declared in outer scopes (a variable's scope in JS is always the nearest function, if any, or otherwise the global scope).

To solve this problem we must "wait" until the first assignment to the newly declared variable occurs. 

### Desired polymorphism

- function polymorphism (function calls instantiate type schemes)
- row type polymorphism


Implementation options:

1. In unification allow also negative rules such as: t != function, and we can use them in assignment expressions (in `f = ...` => f must not be a function)
2. In call expressions, tell the lvalue subexpr that it can instantiate a type scheme. Otherwise, var exprs do not really instantiate type schemes (they don't allow any bound tvars)
3. Switch from type schemes to "forall" as a basic type (higher order types):

example (of option 3):

    makeFunc :: () -> (forall t. t -> t)
    obj.a = makeFunc() // will get: obj.a :: forall t. t -> t
    

Examples:

    var x, y, f, g, h;
    x = { a: 1 }; // x has type '{ "a": number, ... }'
    x = 2; // type error
    y = { a: 2, b: 'bla' }; // y has type '{ "a": number, "b": string, ... }'
    x = y; // OK! x is more general (a la 'subtyping' but not really)
    y = x; // type error - x's type has no "b" field

    // f has type *scheme*: \t -> function(t) : [t]
    f = function(a) { return [a]; };
    f = 2; // type error
    g = f(2); // g has type [number]
    h = f('a'); // h has type [string] -- crucial point! function calls cause instantiation of type scheme



## TODO

- [ ] add "module" primitive that allows vardecls, use it to map ES3 Scripts (should cause trivial.js to pass)
- [ ] support all ES3 statement types (or reject them, but handle all)
- [ ] preserve source code context info and use it for errors
- [ ] support warnings
- [ ] handle funcs with no return statement (should be same as return;)
- [x] get fix.js to infer correctly
- [ ] implement the type system described below
- [ ] function return types should be like vars (use type sigs)
