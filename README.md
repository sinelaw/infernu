# sjs

Safe JavaScript

## TODO

- [ ] add "module" primitive that allows vardecls, use it to map ES3 Scripts (should cause trivial.js to pass)
- [ ] support all ES3 statement types (or reject them, but handle all)
- [ ] preserve source code context info and use it for errors
- [ ] support warnings
- [ ] handle funcs with no return statement (should be same as return;)
- [x] get fix.js to infer correctly
- [ ] implement the type system described below
- [ ] function return types should be like vars (use type sigs)

## Polymorphism and the value restriction

Javascript variables are storage cells. If we compare JS to Standard ML, JS variables are equivalent to `ref a` values - they are not names bound by `let` expressions (a possibly confusing point is that the new version of Javascript, ES6, has a new `let` keyword for defining variables in a block scope. The new `let` also creates storage cells, and isn't equivalent to SML's `let`).

A variable's type can't be determined at declaration time (`var x;`). Only when the variable is assigned `x = expr` we can infer its type. The declaration serves simply to bind the variable's name to the current scope and to possibly shadow variables declared in outer scopes (a variable's scope in JS is always the nearest function, if any, or otherwise the global scope). The declare-then-assign situation is analogous to a reference cell in SML that is initialized to an empty list: at the point of the `let` binding, the SML variable bound to the ref cell has an unknown type (list of what?) because an empty list is ambiguous.

The difference between JS variables and SML let bindings, is in the information about whether or not the name will be restricted to a monotype. In SML we know that in `let x = ref []`, the name `x` will be a monotype because the value restriction applies. Contrarily, in JS at declaration time `var x;` we don't yet know if this variable will be restricted to a monotype.

To solve this problem we must "wait" until the first assignment to the newly declared variable occurs. 

## Outline (TODO)

- polymorphism: no variable polymorphism (variable assignments must have the same concrete type), but yes function polymorphism (function calls instantiate type schemes)
- row type polymorphism (not implemented)

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


