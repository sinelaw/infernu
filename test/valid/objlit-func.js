var f = function(obj, x) {
    return obj.doStuff(x);
};

var testObj = {bla: 3, doStuff: function(r) { return false;}};

f(testObj, 'b');

f = function(obj, x) {
    return obj.doStuff('a'); // ignores x, uses 'a' instead.
};

