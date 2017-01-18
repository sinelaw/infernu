# ![Infernu](docs/infernu.png)

Static type inference for JavaScript.

# MOTHBALLED

## Features

* It's just JavaScript: runs as-is in your browser. No transpilation or compilation required for running.
* Full type inference: no type annotations necessary.
* Safety: a strict type system with no workarounds sets a high bar for code correctness. *So you can sleep at night!*
* **Work in progress**: it can set your computer on fire!

### Type System

* Parametric polymorphism (aka "generics"), based on Hindley-Milner type inference.
* Row-type polymorphism, otherwise known as "static duck typing".
* Simple type classes (which allow for example correct support of JS `+` and `[]` operators).
* Recursive types for true representation of object-oriented methods.
* Correct handling of JS's `this` dynamic scoping rules.

For more information see [Infernu's Type System](docs/type-system.md).

Also see the [intro blog post](https://noamlewis.wordpress.com/2015/01/20/introducing-sjs-a-type-inferer-and-checker-for-javascript/) for a short discussion comparing infernu to **other type checkers**.
 

## Installation

### Quick and Dirty

    git clone git@github.com:sinelaw/infernu.git
    cd infernu/
    cabal install

Usage: see `infernu --help`

Quick example usage:

    echo 'function getLength(x) { return x.length; }' > getLength.js

    infernu getLength.js

Output:

```javascript
    //       getLength : a.({length: b, ..c} -> b)
    function getLength(x) { return x.length; }
```


### A bit more detailed instructions

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

### Running tests

    cd infernu
    cabal sandbox init # if you haven't already
    cabal install --only-dependencies
    cabal build
    cd test
    ./test.sh

Currently there are still a few failing tests due to unclosed issues in the type system.

The file `test/fail.txt` records the test failures and is kept in git. This makes it easier to track progress on outstanding bugs.

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

## Author

Noam Lewis - [@noam_lewis](https://twitter.com/noam_lewis)

## Road map

The following needs to be done to make infernu reasonably usable:

- Finish support for basic builtin JS apis.
- Add support for browser / DOM / web apis.
- Add ability to declare types for, or wrap external libraries.
- Add support for some kind of module system.
- Better error messages.

## Pending discussion

Things that could be done, but may not be so important:

- [ ] Allow empty var decls (use first assignment as starting point for types) - how to prevent uninitialized variable issues? General approach should be to add a translation pass that moves var decls down to the first assignment, but care must be taken to avoid escaping usage-before-assignment to an otherwise shadowed name.
- [ ] When concluding that two recursive types are equivalent, use that information to simplify the resulting types (perhaps using the simpler of the two everywhere) - nice to have, because currently recursive types are displayed opaquely anyway.

More important but also more complicated or impossible to implement:

- [ ] Find a reasonable solution for optional parameters - perhaps using an implicit "Maybe"-like type or implicit type unions, and require guards?
- [ ] An (implicitly inferred) maybe type, for better safety of things like array access at index. Unfortunately because the maybe wrap/unwrap will have to implicit, this isn't easy to solve. See branch `maybe`.
- [ ] Sum types with guards as pattern matchers. Required because some functions, like array index access, can return 'undefined' (e.g. if index is out of range) - breaks parametricity and complicates the inference greatly.
- [ ] Allow defining constructor-object properties using the notation `obj.prototype.something = ...` - requires non-local context to deteremine the type of a constructor function.
- [ ] support `arguments` (a tuple?) and function `bind`
- [ ] Should we treat functions as objects with properties? the only properties they have are: length (very weird! we might as well leave it out), and call/bind/apply (which need special handling)

### Future

- [ ] Type annotations
- [ ] Add support for CommonJS modules

p.s.  *(Formerly known as Inferno / Safe JS / SJS)*

<!--  LocalWords:  JS polymorphism Hindley Milner JS's Equi num arrNums Number String getData
 -->
<!--  LocalWords:  useThisData Undefined unifiable makeData TODO decls paren CommonJS
 -->
