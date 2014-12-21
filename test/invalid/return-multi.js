function f() {
    var g = 'bla';
    if (false) {
        return 2;
    }
    return 3;
    g = 'momo';
    return g;
}

var x = f();
var y = 2;
y = x;
