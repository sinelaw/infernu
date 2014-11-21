var x = function (a) { return a; };
var y = x('a');
x = function (a) { return false; };
x;

