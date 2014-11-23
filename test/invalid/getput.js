function getput(_) {
    var x = [];
    //return [
    //    function(_) { return x; },
    return    function(y) { var old = x; x = y; return old; };
    //];
};

var gp = getput(false);
gp([1]);
gp([false]);
gp;
//gp.put(2);
//var num = gp.get();

//gp.put('a'); // should fail!
