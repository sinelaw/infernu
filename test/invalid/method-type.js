function test(o) { return o.method(2); }
test;
var y = test({method: function(x) { return x; }});
y='a';
