function getput() {
    var x;
    return {
        get: function() { return x; },
        put: function(y) { var old = x; x = y; return old; }
    };
};

var gp = getput();

//gp.put(2);
//var num = gp.get();

//gp.put('a'); // should fail!
