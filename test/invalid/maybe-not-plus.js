function f(x, i) {
    var y = x[i];
    var z; // unspecified, so z :: Maybe b
    if (typeof y !== 'undefined') {
        // will fail, because in this scope "y" has some type a (not ?a) and y + y :: Plus a => a, which doesn't unify with z :: Maybe b
        z = y + y;
    }
}
