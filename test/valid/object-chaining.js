function Obj(y) {
    this.x = y;
    this.doIt = function(x) { this.x = x; return this; };
}
var o = new Obj(8);
o.doIt(2);
o.doIt(2).doIt(3);
//var o2 = new Obj('j');
//o2.doIt('k');
