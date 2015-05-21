function f(x, i) {
    var y = x[i];
    if (typeof y !== 'undefined') {
        return y + y;
    }
}

var foo = 2;

// f should return ?number
foo = f([1,2,3], 0);
