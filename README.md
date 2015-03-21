# Infernu, type inference and checking for JavaScript

*(Formerly known as Inferno / Safe JS / SJS)*

A type inference and checker for JavaScript.

This project is an ongoing effort to produce a practical tool for statically verifying JavaScript code. The type system is designed to support a **safe subset of JS**, not a super set of JS. That is, some otherwise valid JS code will not pass type checking with Infernu. The reason for not allowing the dynamic behavior of JS, is to **guarantee more safety** and (as a bonus) allows fully unambiguous type inference.

See the [intro blog post](https://noamlewis.wordpress.com/2015/01/20/introducing-sjs-a-type-inferer-and-checker-for-javascript/) for a short discussion comparing infernu to **other type checkers**.

**Features:**

* Full type inference: no type annotations necessary.
* Parametric polymorphism (aka "generics"), based on Hindley-Milner type inference.
* Row-type polymorphism, otherwise known as "static duck typing".
* Simple type classes (which allow for example correct support of JS `+` and `[]` operators).
* Recursive types for true representation of object-oriented methods.
* Correct handling of JS's `this` dynamic scoping rules.

For more information see [Infernu's Type System](docs/type-system.md).

Support for type annotations for specifically constraining or for documentation is planned. 

Polymorphism is value restricted, ML-style.

Equi-recursive types are constrained to at least include a row type in the recursion to prevent inference of evil recursive types.


## Examples

**Note**: An ongoing goal is to improve readability of type signatures. 

### Basic

JavaScript:

	var num = 2;
	var arrNums = [num, num];

Infernu infers (for arrNums):

	[TNumber]

That is, an array of numbers.

Objects:

	var obj = { something: 'hi', value: num };

Inferred type:

    {something: TString, value: TNumber}

That is, an object with two properties: 'something', of type string, and 'value' of type number.

### Functions and `this`

In JS, `this` is one truly awful part. `this` is a dynamically scoped variable that takes on values depending on how the current function was invoked. Infernu knows about this (pun intended) and infers types for functions indicating what `this` must be.

For example:

	function useThisData() {
		return this.data + 3;
	}

Infernu infers:

    {data: TNumber, ..l}.(() -> TNumber)

In words: a function which expects `this` to be an object with at least one property, "data" of type number. It takes no arguments (hence the empty `()`). It returns a number.

If we call a function that needs `this` incorrectly, Infernu will be angry:

    > useThisData();
	Error: Could not unify: {data: TNumber, ..a} with TUndefined

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

The basic example is for the `+` operator:

    function add(x,y) { return x + y; }

The type for `add` is:

    Plus a => (a,a) -> a

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

<!--  LocalWords:  JS polymorphism Hindley Milner JS's Equi num arrNums TNumber TString getData
 -->
<!--  LocalWords:  useThisData TUndefined unifiable makeData TODO decls paren CommonJS
 -->
