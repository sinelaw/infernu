function f(x, i, def) {
    var y = x[i];
    if (typeof y !== 'undefined') {
        return y + y;
    }
    return def;
}

f([1,2,3], 0, -1);

f({'mo': 3 }, 'mo', 2);
