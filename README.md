# sjs

Safe JavaScript


## TODO

- [ ] find a new name!
- [ ] find a way to deal with methods that don't restrict "this" so that they can be calling them without this doesn't restrict them to 'undefined'
- [ ] consider allowing empty var decls (use first assignment as starting point for types) - how to prevent uninitialized variable issues?
- [ ] in inferred types, preserve source code context info and use it for more readable unification errors
- [ ] allow defining constructor-object properties using the notation `obj.prototype.something = ...`
- [ ] treat arrays and functions as objects with properties
- [ ] when concluding that two recursive types are equivalent, use that information to simplify the resulting types (perhaps using the simpler of the two everywhere)

### Future

- [ ] type annotations
- [ ] add support for CommonJS modules
