function Obj() {
    this.x = 0;
    this.doIt = function(x) { this.x = x; return this; };
}
var o = new Obj();
o.doIt(2).doIt(3);
