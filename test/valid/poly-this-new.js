
function MkObj() {
    this.m = function(x) { return x; };
}
var obj1 = new MkObj();
var obj2 = new MkObj();

obj1.m(2);

var g = obj2.m;
g(3);
