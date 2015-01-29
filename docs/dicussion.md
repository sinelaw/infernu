**Note**: Most of this is outdated and/or unmaintained.

# Discussions

## AST translation to core language: yes or no?

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

Javascript variables are storage cells. If we compare JS to Standard ML, JS variables seem to be equivalent to `ref` values. SML has the concept of value restriction: but it applies to let-bound names, and `ref` variables in SML are *always* monotypes. If we followed suit when inferring JS variable types, all our variables would be monotypes and we would lose all polymorphism.

A safe behavior for a JS variable can be either a monotype (`'a ref` in ML) or, when storing polymorphic values (such as an empty list or polymorphic functions), something more like an ocaml record with a mutable cell with a quantified type variable (forall).

A mutable monotype variable has the same type as ocaml's `ref`, which is:

```ocaml
type 'a ref = { mutable content : 'a } ;;
```

Here's (in ocaml) a mutable variable that can only contain the polymorphic empty list. The inner record field has a `forall` quantifier on the type of the list item.

```ocaml
type t1 = { mutable v1 : 'a. 'a list } ;; (* notice the quantifier 'a. *)

let x = { v1 = [] } ;;
x.v1 <- [] ;; (* not too many things we can put in there... *)
```

More interesting is the type of a mutable variable that contains polymorphic functions of type `'a -> 'a list` (or `a -> [a]` if you prefer):

```ocaml
type t2 = { mutable v2 : 'a. 'a -> 'a list } ;;

let y = { v2 = fun x -> [ x; x; ] };;
y.v2 3;; (* evaluates to [ 3; 3 ]*)
y.v2 'a';; (* [ 'a'; 'a' ] *)

y.v2 <- fun x -> [ x; x; x; ] ;;
y.v2 3;; (* [ 3; 3; 3 ] *)
etc..
```

### Possibly solutions to polymorphism of mutable variables

1. Infer types, and allow full polymorphism of mutable variables, allowing even nonsense like `x = 'a'; x = 3;`, which makes this solution useless.
2. Infer types, and disable polymorphism (treating JS vars like ML `ref`s). Variables are still allowed to be of types where **`a` is free** (such as `a -> a`), but not allowed to be of **closed types** (such as `forall a. a -> a`).
3. Don't infer types - require programmer to annotate code with type ascriptions. Interpret all type variables as universally quantified, e.g. `a` is interpreted as `forall a. a` (and no value can ever be assigned) or more usefully, `a -> [a]` will be interpreted as `forall a. a -> [a]` (and many list-constructing functions will inhabit this type). This approach is similar to ocaml's **mutable record fields**.
4. Infer types, but allow polymorphism only in certain cases. **TODO: Which cases?**


### Examples

Consider the following examples:

```javascript
var x = 1;
x = 'a'; // should not type check
```

Here we have assigned two types to the same variable (recall that variables are reference cells), which should not be allowed.

```javascript
var x = [];
x = [1];
x = ['a']; // should not type check
```

Here again we used two types. Unlike the previous example, we initially assign a value with unknown type (`[a]`, but we don't know what `a` is).

```javascript
var x;
x = 1;
x = 'a'; // should not type check
```

In the above example we have no information at all about `x`'s type at declaration time. There is no analogy for this in SML-like languages.

```javascript
var f = function (x) { return x; }
var n = f(1);
var s = f('a'); // ok
```

Here we have assigned a polymorphic function (of type `a -> a`) to the variable `f`. Later we invoke this function twice, using different types each time, which should be allowed. This is an example of desired polymorphism.

```javascript
var f = function() { return function(x) { return x; } }
var g = f(); // g should also be polymorphic
var n = g(1);
var s = g('a'); // this is ok, but value restriction would not allow this to type check
```

Here again we make use of polymorphism. However, because we're assigning `g` from an expression that isn't a syntactic value (`f()` is a function invocation), languages such as SML will restrict `g` to a monotype and unify `g` to type `number -> number` after the call to `g(1)`. When designing our type system we must consider applying this limitation, to avoid other problems that the value restriction was designed to avoid.

```javascript
var x = 1;
var f = function(y) { var res = x; x = y; return res; } // should get type: number -> number
```

The above function, in pure (dynamically typed) JS will return the value that was passed "last time" the function was called, regardless of its type. With unmodified HM, the inferred type is `number -> number` because `x` has been assigned a number `1`, and everything is fine. What should happen when `x` is not assigned (only declared)?

```javascript
var x;
var f = function(y) { var res = x; x = y; return res; } // type ???
```

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


# Normalizing recursive types on outermost scope

    t = { method: t, x: String } -> Int
        { method: { method: t, x: String } -> Int, x: String } -> Int

    => o = { method: t, x: String }

        t = o -> Int

    => o = { method: o -> Int, x: String }

        { method: o -> Int, x: String } -> Int
        o -> Int


# Higher-rank polymorphism for object methods

See:

- http://blog.huoc.org/higher-rank-polymorphism.html
- "Semi-Explicit First-Class Polymorphism for ML" (1999), by Jacques Guarrigue and Didier Rémy. (1999) http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.4858

