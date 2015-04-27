var o2 =  { method: function(x) { return x; } };

var x = o2.method('a'); 

// requires o2.method to become more specific (Number -> Number)
// should not work.
o2.method = function (x) { x = 2; return x; };


