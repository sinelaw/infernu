/* @flow */

function memoize(f) {
    var mem = {};

    return function(x) {
        var key = String(x);
        mem[key] = mem[key] || function(){return f(x);};
        return mem[key]();
    };
}

var test = memoize(function(x) { return x + 2; });

test('a');
