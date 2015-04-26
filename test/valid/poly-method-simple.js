
var obj = { id: function(x) { return x; } };

function doId(o,x) { return o.id(x); }

doId(obj, 2);
