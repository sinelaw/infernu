var x = [];
var getX = function (v1) { return x; }; // forall b. X = forall b X(a). X(a)
var setX = function (v2) { x = v2; return true; }; // X -> Bool
setX([1]); // X ~ forall c. c -> String  ==>  forall a. a -> a ~ forall c. c -> String  ==>  a=c, a=String  ==>  X = String -> String
getX(false);
setX([false]);

