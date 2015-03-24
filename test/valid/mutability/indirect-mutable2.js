function x(q){return q;};
var y = x;
y = function(q){return false;};
x(2);
x;
y;
