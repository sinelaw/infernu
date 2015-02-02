var o2 =  { method: function(x) { x = 2; return x; } };

// Should succeed because it's more general than Number -> Number
o2.method = function (x) { return x; };

