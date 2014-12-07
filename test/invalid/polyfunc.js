var f = function(x) { return function(y) { return y[x]; }; };
var num = f([3])([2]);
f;

