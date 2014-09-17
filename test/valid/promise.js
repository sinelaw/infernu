


function once(f) {
    var ran = true;
    var result;
    return function(x) {
        if (ran) {
            ran = false;
            result = f(x);
        }
        return result;
    };
}

//var g = once(function(x) { return x; })

var h = once(function(x) { return 'shimon'; });
//var j = g('moshe');

//g(2);
//h(false);
