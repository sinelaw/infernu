function bar(objArg) { return objArg.foo(5) == 2; }
var obj = { foo : function(x) { return x; } };
bar(obj);
