var f = function(x, y) { return function(z) { x = z; return y; }; };
f([2], 'a')([3]);
f;

