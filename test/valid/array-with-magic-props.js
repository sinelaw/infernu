function f1(x) { return x.o; }
function f2(x) { return x[0]; }

var g = [];
f1(g[0]);
f2(g[0]);
