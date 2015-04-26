
var obj = { getIdHolder: function() { return { id: function(x) { return x; } }; } };

function doId(o,x) { return o.getIdHolder().id(x); }

doId(obj, 2);
