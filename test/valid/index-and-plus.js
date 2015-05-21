function f(x, i) {
    var y = x[i];
    if (typeof y !== 'undefined') {
        return y + y;
    }
}

f([1,2,3], 0);

f({'mo': 3 }, 'mo');
