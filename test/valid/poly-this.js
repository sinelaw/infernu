
function makeObj() {
    return { m: function(x) { return x; } }; // 'this' isn't used.
}


var f = makeObj().m;
makeObj().m(2);

f(3);
