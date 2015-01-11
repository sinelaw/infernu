var o = { method: function() {  return this; } };
var x = o.method();
var y = x.method();
var z = x === y;
z;
