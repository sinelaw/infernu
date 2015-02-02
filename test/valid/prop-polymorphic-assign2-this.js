var o2 =  { method: function(x) { x = this; return x; } };

// Should fail because it's less general than forall a. a -> a
o2.method = function (x) { return x; };

