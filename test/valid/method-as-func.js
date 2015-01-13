var o = { method: function() {  return 3; } };
var mo = function(){return o;};
o.method();
var f = o.method;
f();
