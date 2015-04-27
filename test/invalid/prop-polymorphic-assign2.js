var o2 =  { method: function(x) { return x; } };

// causes o2.method to become more specific (Number -> Number)
// and so should fail
o2.method = function (x) { x = 2; return x; };

