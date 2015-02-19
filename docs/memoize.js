/* @flow */

function memoize(toKeyString, f) {
    var mem = {};

    return function memed(x) {
        var key = toKeyString(x);
        mem[key] = mem[key] || [];
        if (mem[key].length < 1) { mem[key] = [f(memed, x)]; }
        return mem[key][0];
    };
}

var facTest = memoize(String, function fac(memed, x) { if (x < 2) { return 1; } return x * memed(x - 1); });


var nkTest = memoize(
    function (x) {
        return String(x.k) + ', ' + String(x.n); },
    function f(m, x) {
        if (x.k === 0) { return 1; }
        if (x.n === 0) { return 0; }
        return m({ n: x.n-1, k: x.k }) + m({ n: x.n, k: x.k-1 }) + m({ n: x.n-1, k: x.k-1 });
    });


nkTest({ n: 30, k: 30 });

