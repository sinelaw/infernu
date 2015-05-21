function f1(x) { return x.o; }
function f2(x) { return x[0]; }

function foo(g) {
    f1(g);
    f2(g);
}
