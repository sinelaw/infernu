function fix(f) { return f(fix(f)); }

var g = fix(function (x) { var y = 0; y = x; return 'a'; });
g;
