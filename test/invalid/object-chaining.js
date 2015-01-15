function Obj(y) {
    this.x = y;
    this.doIt = function(x) { this.x = x; return this; };
}
var o = new Obj(2);
var o2 = o.doIt(2);
var o3 = o2.doIt('a');
