var x = 0;

var f = function(a) { a = x; return a; };


var obj = { method: function(a) { return a; } };

obj.method  = function(a) { a = x; return a; };
