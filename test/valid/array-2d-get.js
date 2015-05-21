function f(x) {
    var a = x[0];
    if (typeof a !== 'undefined') {
        return a[0];
    }
    return undefined;
}
f([[0]]);
