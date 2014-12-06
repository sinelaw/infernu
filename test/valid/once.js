function once(f) {
    var ran = true;
    var result = [];
    return function(x) {
        if (ran) {
            ran = false;
            result = [f(x)];
        }
        return result[0];
    };
}

var h = once(function(x) { return 'shimon'; });
h;
