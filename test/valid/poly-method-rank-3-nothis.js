// supposedly equivalent to this problem:
// -- forall a b. ((() ->            (a -> b)),    a) -> b
// -- forall    . ((() -> (forall d. (d -> d)), Bool) -> Bool

function forceUndef(x) { x = undefined; }

var obj = { getIdHolder: function() { forceUndef(this); return { id: function(x) { forceUndef(this); return x; } }; } };

function doId(o,x) {
    forceUndef(this);
    var f = o.getIdHolder;
    var o2 = f();
    var g = o2.id;
    return g(x);
}

doId(obj, 2);
