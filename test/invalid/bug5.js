var x = function (a) { return a; }; // X = forall a. a -> a
var getX = function (v1) { return x; }; // forall b. X = forall b X(a). X(a)
var setX = function (v2) { x = v2; return true; }; // X -> Bool
setX(function(a2) { return 'a'; }); // X ~ forall c. c -> String  ==>  forall a. a -> a ~ forall c. c -> String  ==>  a=c, a=String  ==>  X = String -> String
getX(false)(false);

