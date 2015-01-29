
function f(x) { return x; }
var obj = { m: f }; // 'this' isn't used.

var g = obj.m;
obj.m(2);

g(3);
