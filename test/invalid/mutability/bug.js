var x = function(a) { return a; };
function setX(v) { x = v; return false; }
setX(function (a) { return 'a'; });
setX;
x = function(a) { return 2; };
