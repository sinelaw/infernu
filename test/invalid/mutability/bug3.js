var x = function (a) { return a; };
var y = x('bla');
x = function (b) { return false; }; // should cause an error, because x was already used as a string -> string
x;

