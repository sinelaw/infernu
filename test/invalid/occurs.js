//[a] -> [a]
var f = function (x) {
//    [a, z -> [z]] 
    var bla = [x[0], function (z) {
        return f([z]); }];
    return x;
};
f;

f([false]);

