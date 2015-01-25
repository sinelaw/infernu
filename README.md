# SJS, for Safe JavaScript

A type inference and checker for JavaScript.

This project is an ongoing effort to produce a practical tool for statically verifying JavaScript code. The type system is designed to support a **safe subset of JS**, not a super set of JS. That is, some otherwise valid JS code will not pass type checking with SJS. The reason for not allowing the dynamic behavior of JS, is to **guarantee more safety** and (as a bonus) allows fully unambiguous type inference.

**Features:**

* Full type inference: no type annotations necessary.
* Parametric polymorphism (aka "generics"), based on Hindley-Milner type inference.
* Row-type polymorphism, otherwise known as "static duck typing".
* Recursive types for true representation of object-oriented methods.
* Correct handling of JS's `this` dynamic scoping rules.

Support for type annotations for specifically constraining or for documentation is planned. 

Polymorphism is value restricted, ML-style.

Equi-recursive types are constrained to at least include a row type in the recursion to prevent inference of evil recursive types.


## Example

**Note**: An ongoing goal is to improve readability of type signatures. 

### Basic

JavaScript:

	var num = 2;
	var arrNums = [num, num];

SJS infers (for arrNums):

	[TNumber]

That is, an array of numbers.

Objects:

	var obj = { something: 'hi', value: num };

Inferred type:

    {something: TString, value: TNumber}

That is, an object with two properties: 'something', of type string, and 'value' of type number.

### Functions and `this`

In JS, `this` is one truly awful part. `this` is a dynamically scoped variable that takes on values depending on how the current function was invoked. SJS knows about this (pun intended) and infers types for functions indicating what `this` must be.

For example:

	function useThisData() {
		return this.data + 3;
	}

SJS infers:

    (this: {data: TNumber, ..l} -> TNumber)

In words: a function which expects `this` to be an object with at least one property, "data" of type number. It returns a number.

If we call a function that needs `this` incorrectly, SJS will be angry:

    > useThisData();
	Error: Could not unify: {data: TNumber, ..a} with TUndefined

Because we called `useThisData` without a preceding object property access (e.g. `obj.useThisData`), it will get `undefined` for `this`. SJS is telling us that our expected type for `this` is not unifiable with the type `undefined`.

### Polymorphism

Given the following function:

    function makeData(x) {
	    return {data: x};
	}

SJS infer the following type:

    ((this: a, b) -> {data: b})

In words: A function that takes anything for its `this`, and an argument of any type, call it `b`. It returns an object containing a single field, `data` of the same type `b` as the argument.

### Row-type polymorphism (static duck typing)

Given the following function:

    function getData(obj) {
		return obj.data;
	}

SJS infers:

    ((this: h, {data: i, ..j}) -> i)

In words: a function taking any type for `this`, and a parameter that contains **at least one property**, named "data" that has some type `i` (could be any type). The function returns the same type `i` as the data property.

TODO: More examples

------------

## TODO

- [ ] find a new name!
- [ ] find a way to deal with methods that don't restrict "this" so that calling them without this doesn't restrict it to 'undefined'
- [ ] consider allowing empty var decls (use first assignment as starting point for types) - how to prevent uninitialized variable issues?
- [ ] in inferred types, preserve source code context info and use it for more readable unification errors
- [ ] allow defining constructor-object properties using the notation `obj.prototype.something = ...`
- [ ] deal with polymorphic object properties (e.g. array.map)
- [x] treat arrays as objects with properties
- [ ] treat functions as objects with properties
- [ ] when concluding that two recursive types are equivalent, use that information to simplify the resulting types (perhaps using the simpler of the two everywhere)
- [ ] top-level type of naked object `{a:3}` isn't shown unless it is wrapped in a paren `({a:3})`.
- [ ] support `arguments`, `call` and `bind`

### Future

- [ ] type annotations
- [ ] add support for CommonJS modules

<!--  LocalWords:  SJS JS polymorphism Hindley Milner JS's Equi num arrNums TNumber TString getData
 -->
<!--  LocalWords:  useThisData TUndefined unifiable makeData TODO decls paren CommonJS
 -->
