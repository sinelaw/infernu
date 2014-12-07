var f = function(x, y) { return function(z) { x = z; return y; }; };
f(1, 'a')(['b']);
f;

