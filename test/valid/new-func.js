function mkn(f, a) { return new f(a); }

mkn;

function Foo(x) { this.bla = x; }

mkn(Foo, 2);

