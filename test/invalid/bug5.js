var x = function (a) { return a; };     // forall a. a -> a            | c -> c                    | (String -> String)
var getX = function (v1) { return x; }; // forall b a. b -> (a -> a)   | forall b. b -> (c -> c)   | forall b. b -> (String -> String)
var setX = function (v2) { x = v2; return true; }; // x is mutable =>  | (c -> c) -> Bool          | (String -> String) -> Bool
setX(function(a2) { return 'a'; }); //                                                             | c => String 
getX(false)(false);

