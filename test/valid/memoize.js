/* @flow */
// 'f' is a function that takes a recursion function, and the current value. It should call the recursion function instead of itself when it needs to recurse.
function memoize(toKeyString, f) {
    var mem = {};

    return function memed(x) {
        var key = toKeyString(x);
        mem[key] = mem[key] || f(memed, x);
        return mem[key];
    };
}

var facTest = memoize(String, function fac(memed, x) { if (x < 2) { return 1; } return x * memed(x - 1); });

// The number of n-dimensional vectors whose scalar's sums to k
// Hat-tip to https://stackoverflow.com/questions/3242597/what-is-memoization-good-for-and-is-it-really-all-that-helpful
var nkTest = memoize(
    function (x) {
        return String(x.k) + ', ' + String(x.n); },
    function f(m, x) {
        if (x.k === 0) { return 1; }
        if (x.n === 0) { return 0; }
        return m({ n: x.n-1, k: x.k }) + m({ n: x.n, k: x.k-1 }) + m({ n: x.n-1, k: x.k-1 });
    });


nkTest({ n: 30, k: 30 });

