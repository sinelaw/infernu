var a = [1,2,3];
var b = ['a','b','c'];

var a1 = a.map(function(x,i,ar) { return String(x); });
var b1 = b.map(function(x,i,ar) { return parseInt(x, 10); });
