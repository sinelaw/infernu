function f() { return this.x; }
var res = f.call({x:3});
res = 2;
var res1 = f.call({x:'a'});
res1 = 'b';
