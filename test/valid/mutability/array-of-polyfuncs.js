var arr = [function(x) { return x; }];
// because 'arr' is not generalized, the next line causes arr to be the monomorphic type [a.(Number -> Number)] where 'a' is free (undetermined so far, but not quantified)
arr.push(function(x) { return 0; });
// here the call uses 'undefined' for 'this', so the 'a' tvar is unified with Undefined, leaving arr with type [Undefined.(Number -> Number)]
arr[0](1);

//  will give us: obj : {f: Undefined.(Number -> Number)}
var obj = { f: arr[0] };

// passes obj as 'this' to 'f', so requires the compiler to allow passing any type when Undefined is expected as an argument
obj.f(1);
