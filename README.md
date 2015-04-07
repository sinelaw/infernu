# Infernu

Static type inference for JavaScript.

See the [intro blog post](https://noamlewis.wordpress.com/2015/01/20/introducing-sjs-a-type-inferer-and-checker-for-javascript/) for a short discussion comparing infernu to **other type checkers**.

*(Formerly known as Inferno / Safe JS / SJS)*

**Features:**

* Full type inference: no type annotations necessary.
* Parametric polymorphism (aka "generics"), based on Hindley-Milner type inference.
* Row-type polymorphism, otherwise known as "static duck typing".
* Simple type classes (which allow for example correct support of JS `+` and `[]` operators).
* Recursive types for true representation of object-oriented methods.
* Correct handling of JS's `this` dynamic scoping rules.

For more information see [Infernu's Type System](docs/type-system.md).

## Installation

1. Install Haskell's **cabal** package manager. See [Haskell.org](https://www.haskell.org/downloads) for some installation options. On ubuntu, I recommend using [Herbert V. Riedel's ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc).
2. Clone this repository.

Then run:

    cabal update
    cd infernu
    cabal install

The `infernu` executable will be installed to your `~/.cabal/bin`. You may want to add it to your `PATH`.

If you have trouble in the last command due to package incompatibilities, use a **cabal sandbox**:

    cd infernu
    cabal sandbox init
    cabal install

The `infernu` executable will be placed in `infernu/.cabal-sandbox/bin`


## Examples

### Basic

JavaScript:

	var num = 2;
	var arrNums = [num, num];

Infernu infers:

    //  num : Number
    //  arrNums : [Number]

That is, an array of numbers.

Objects:

	var obj = { something: 'hi', value: num };

Inferred type:

    //  obj : {something: String,
               value: Number}

That is, an object with two properties: 'something', of type string, and 'value' of type number.

### Functions and `this`

In JS, `this` is one truly awful part. `this` is a dynamically scoped variable that takes on values depending on how the current function was invoked. Infernu knows about this (pun intended) and infers types for functions indicating what `this` must be.

For example:

	function useThisData() {
		return this.data + 3;
	}

Infernu infers:

    //       useThisData : {data: Number, ..a}.(() -> Number)

In words: a function which expects `this` to be an object with at least one property, "data" of type `Number`. It takes no arguments (hence the empty `()`). It returns a `Number`.

If we call a function that needs `this` incorrectly, Infernu will be angry:

    Error: Could not unify:
        {data: Number, ..a}
      With:
        Undefined

Because we called `useThisData` without a preceding object property access (e.g. `obj.useThisData`), it will get `undefined` for `this`. Infernu is telling us that our expected type for `this` is not unifiable with the type `undefined`.

### Polymorphism

Given the following function:

    function makeData(x) {
	    return {data: x};
	}

Infernu infers the following type:

    a.(b -> {data: b})

In words: A function that takes anything for its `this`, and an argument of any type `b`. It returns an object containing a single field, `data` of the same type `b` as the argument.

### Row-type polymorphism (static duck typing)

Given the following function:

    function getData(obj) {
		return obj.data;
	}

Infernu infers:

    h.({data: i, ..j} -> i)

In words: a function taking any type `h` for `this`, and a parameter that contains **at least one property**, named "data" that has some type `i` (could be any type). The function returns the same type `i` as the data property.


### Type Classes

See [here](docs/type-system.md#type-classes) for more about Infernu's type classes.

The basic example is for the `+` operator:

    function add(x,y) { return x + y; }

The type for `add` is inferred to be:

    //       add : Plus b => a.((b, b) -> b)

Meaning: given any type `a` that is an instance of the `Plus` type class, the function takes two `a`s and returns an `a`.

The two instances of `Plus` currently defined are the types `Number` and `String`.



------------

## TODO

- [ ] consider adding sum types with guards as pattern matchers. required because some functions, like array index access, can return 'undefined' (e.g. if index is out of range)
- [ ] allow empty var decls (use first assignment as starting point for types) - how to prevent uninitialized variable issues?
- [ ] allow defining constructor-object properties using the notation `obj.prototype.something = ...`
- [ ] find a reasonable solution for optional parameters - perhaps using an implicit "Maybe"-like type or implicit type unions, and require guards?
- [ ] when concluding that two recursive types are equivalent, use that information to simplify the resulting types (perhaps using the simpler of the two everywhere)
- [ ] BUG: top-level type of naked object `{a:3}` isn't shown unless it is wrapped in a paren `({a:3})`.
- [ ] support `arguments` (a tuple?) and function `bind`
- [ ] Should we treat functions as objects with properties? the only properties they have are: length (very weird! we might as well leave it out), and call/bind/apply (which need special handling)

### Future

- [ ] type annotations
- [ ] add support for CommonJS modules
- [ ] deal better with inferred polymorphic object properties - requires full rank-n unification

<!--  LocalWords:  JS polymorphism Hindley Milner JS's Equi num arrNums Number String getData
 -->
<!--  LocalWords:  useThisData Undefined unifiable makeData TODO decls paren CommonJS
 -->
