function mkn(f, a) { return new f(a); }

mkn;

function Foo(x) { this.bla = x; }

var x = mkn(Foo, 2);

x;

var y = mkn(Foo, false);

y;
