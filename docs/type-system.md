# Infernu's Type System

## Overview

Infernu is a type checker for JavaScript. Since JavaScript is dynamically and weakly typed, it makes no sense to talk about "type errors" in arbitrary JavaScript code. Consequently Infernu makes assumptions about the code and expects it to follow certain rules that are not required by plain JavaScript (for example, implicit coercions such as `3 + 'a'` are not allowed.)

Infernu's type system is designed for writing dynamic-looking code in a safe statically type-checked environment. Type annotations are not required (though they would be nice to support, for various reasons). Instead, Infernu *infers* the types of expressions by examining the code. If the inferred types contradict each other, Infernu reports the contradiction as an error.

Infernu places restrictions on JS programs that are otherwise valid. In other words, Infernu is a **subset of JavaScript**. Infernu tries to strike a balance between type system complexity and dynamic-style coding flexibility.

**Notation:** Concrete types are capitalized; type variables are lowercase letters (a, b, c, etc.)

### For type system geeks

Infernu's type system is based on Damas-Hindley-Milner (and Algorithm W) with added mutable variables (monomorphically constrained using value restriction), and with row type polymorphism, equi-recursive types (constrained to go through row types), rank-2 row types (for polymorphic methods), and simple type classes.

## Primitive Types

* `Number`
* `Boolean`
* `String`
* `Regex`
* `Undefined`
* `Null` (may be removed in the future)

(`Date` should probably be added)

## Generics, or Parametric Polymorphism

Parameterized types take one or more type parameters. Infernu can infer about code that it requires a certain parameterized type without necessarily fixing the parameter. For example, `a[0] = x` may be interpreted as "assignment to an array of elements which have the same type as `x`" (actually Infernu places a weaker assumption on such code - see *type classes* below).

## Type Variables

Type variables represent a type that is not fully constrained to any particular type. A type variable can represent a *partial* constraint:

* The same type variable can be referred to multiple times, constraining all those types to be the same. For example, `a -> a` - a function returns the same type as its argument.
* Sometimes a type variable is constrained by a `type class predicate`, such as `Plus a => a -> a -> a` (the type of the `+` operator). See *type classes* below.


### Quantified vs. Unknown Type Variables

There are two cases were type variables are needed: 

* A *quantified* type variable is **allowed to vary**. For example, a function that can take any type for its argument and returns a String, can be represented as `a -> String` but the type variable `a` must be allowed to vary. Being "allowed to vary" means: if we use this function in a way that forces `a` to be, say, the type `Number`, doesn't mean that now `a` will always be `Number`: other pieces of code can use our function in a different way, having `a` be whatever other type. A type variable such as this `a` is known as *universally quantified* (or "foralled"). The type of that function is really `forall a. a -> String`, or "given any type, call it `a`, this function will have the type `a -> String`" (Currently the pretty-printing mechanism of Infernu doesn't print out the "forall" parts).
* A *free* (or non-quantified) type variable is used when a certain type is not known. If we add more code to the program we may end up determining what the type variable should represent, for example some type variable `b` that appears throughout the code before the change, may turn out to be a `String` due to a new line of code that uses it as such.

Free type variables are not very useful, but they represent code that can be made generic (for example by wrapping it in a function).

## Built-in Parameterized Types

* `[a]` - Arrays of items of type `a`
* `Map a` - Maps from `String` to `a`. Maps correspond to plain JS objects that are used via dynamic keys (e.g. `obj[key] = value`). See the section "row types" for another view of JS objects.

In plain JS, arrays are *heterogeneous* and can contain a mix of values: numbers, booleans, objects, functions, etc. For example, `['a', 3, { b: [] } ]` is a valid JS array. In Infernu, arrays are restricted to be *homogeneous*: all items must be of the same type. 

Arrays, string maps, and plain strings can all be accessed using the bracket notation in JS. Infernu defines instances of all three types for the `Indexable` type class. More on that later.

### Functions

Functions are similar to other parameterized types, but are represented slightly differently internally to support `this` and the fact that they can be used as constructors (when using the `new` keyword). 

Notation:

`a.(b,c,...) -> r` represents functions that take `this` of type `a`, arguments of types `b`, `c`, ... and return a result of type `r`.

For constructors, there are also properties one can tack onto the `.prototype` of a constructor function (support for `.prototype` is still in the works).

## Row types

JavaScript objects can be viewed as either maps from strings (property names) to values, or as records (or "rows") containing a set of fields, each field having a certain type. Infernu supports both of these two views, maps and rows, but represents them as separate (incompatible) types. This section is about the second view of objects, as rows.

The basic notation for row types is: `{ prop1: typeA, prop2: typeB }` where `prop` are strings, names of properties, with their corresponding types.

Row types can be either **closed** or **open**:

* A **closed** row type has a set number of properties with specific names: it is not compatible with any other row.
* An **open** row type is a row that is known to have some properties, but may have (and is compatible with) more properties, as long as their names don't collide with the existing properties. An open row type has a *row type variable* representing the "tail" or remaining properties.

The notation for an open row type is:

`{ prop1: typeA, prop2: typeB, ..r }` where `r` is the row type variable.

### Polymorphic methods / rank-2 row types

## Equi-recursive types

## Type classes


<!--  LocalWords:  Damas Hindley Milner equi foralled forall Indexable
 -->
