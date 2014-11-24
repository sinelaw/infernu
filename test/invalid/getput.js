function getput(_) {
    var x = [];
    return function(y) { var old = x; x = y; return old; };
};

var gp = getput(false);
gp([1]);
gp([false]);
gp;
