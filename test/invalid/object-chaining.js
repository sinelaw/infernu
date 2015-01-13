function Obj(y) {
    this.x = y;
    this.doIt = function(x) { this.x = x; return this; };
}
var o = new Obj(2);
o.doIt(2);
o;
// function wrapDoIt(x) { return o.doIt(x); }
// var o2 = o.doIt(2);
// o2;
//var o3 = o2.doIt('a');
