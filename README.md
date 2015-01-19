# Safe JavaScript

A type inference and checker for JavaScript.

**Features:**

* Full type inference: no type annotations neccessary.
* Parametric polymorphism (aka "generics"), based on Hindley-Milner type inference.
* Row-type polymorphism, otherwise known as "static duck typing".
* Recursive types for true representation of object-oriented methods.
* Correct handling of JS's `this` dynamic scoping rules.

Support for type annotations for specifically constraining or for documentation is planned. 

Polymorphism is value restricted, ML-style.

Equi-recursive types are constrained to at least include a row type in the recursion to prevent inference of evil recursive types.

## Example

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
- [ ] find a way to deal with methods that don't restrict "this" so that they can be calling them without this doesn't restrict them to 'undefined'
- [ ] consider allowing empty var decls (use first assignment as starting point for types) - how to prevent uninitialized variable issues?
- [ ] in inferred types, preserve source code context info and use it for more readable unification errors
- [ ] allow defining constructor-object properties using the notation `obj.prototype.something = ...`
- [ ] treat arrays and functions as objects with properties
- [ ] when concluding that two recursive types are equivalent, use that information to simplify the resulting types (perhaps using the simpler of the two everywhere)
- [ ] top-level type of naked object `{a:3}` isn't shown unless it is wrapped in a paren `({a:3})`.


### Future

- [ ] type annotations
- [ ] add support for CommonJS modules
