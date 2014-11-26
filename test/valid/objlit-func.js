var f = function(obj, x) {
    return obj.doStuff(x);
};

f({bla: 3, doStuff: function(r) { return false;}});

f = function(obj, x) {
    return obj.doStuff('a'); // ignores x, uses 2 instead.
};


f;

// f; // :: (a -> ({doStuff: (a -> b), ...} -> b))
