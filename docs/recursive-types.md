We are translating an object-oriented language to a "mini ocaml", i.e. a typed lambda calculus with let bindings and row-type polymorphism.

Consider the expression:

    obj.set(obj.get())

Which is translated to:

    obj#set obj (obj#get obj)

Or if uncurried:

    obj#set(obj, obj#get(obj))

`#` being the record field accessor.

![](https://github.com/sinelaw/infernu/blob/kind/docs/images/inference.dot.png)

Note that we pass `obj` itself to each of its methods (hence the recursive types). DHM-style type inference begins at the outer expression, the application of `obj#set` to the parameters `(obj, obj#get(obj))`. Recursing into `obj#set` infers the type of `obj` first:

    obj : a

Then of the property read `obj#set`:

    set : b

Then, stepping out to the field accessor expression we infer that `obj` is a record with a field get of the aforementioned type:

    obj : a = { set : b | r1 }

Where `r1` is a fresh row-polymorphic type variable.

We continue outward towards the application of the function `obj#set`, and then down into the arguments. We already have the following type for the first argument, `obj : { set : b | r1 }`. We proceed to infer the type of the application `obj#get obj`. On the left-hand-side we have field access, which now unifies the type:

    { get : c | r2 }

With the previously inferred type for `obj`,

    { set : b | r1 }

Which unify to:

    obj : { set : b, get : c | r2 }

Now, here's where type recursion occurs. Because of the application of `obj#get` on `obj`, we must infer the type of the `get` field to be a function:

    get : c = { set : b, get : c | r2 } -> d

Notice that `c` is a recursive type: it appears in its own substitution.

![](https://github.com/sinelaw/infernu/blob/kind/docs/images/inference-get.dot.png)

Stepping out when level, we get to the application of `obj#set (obj, obj#get obj)`. We infer `set` to be a function:

    set : b = { set : b, get : c | r2 } -> e

Now `b` turns out to also be recursive. Here is the full type of obj:

![](https://github.com/sinelaw/infernu/blob/kind/docs/images/inference-getset.dot.png)

Hence, we have mutually equi-recursive types, `b` and `c`, the types of the fields `set` and `get`.

