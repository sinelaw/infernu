function fmap(m, f) { if (typeof m !== 'undefined') { return f(m); } else { return undefined; } }

var x;
x = 3;

var z = fmap(x, function(y) { return 'foo'; });
z = undefined;
