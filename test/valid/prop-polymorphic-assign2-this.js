var o2 =  { method: function(x) { x = this; return x; } };

// Should succeed because it's more general than forall a. (this: a, a) -> a
o2.method = function (x) { return x; };

