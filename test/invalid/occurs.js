
var f = function (x) { 
    var bla = [x[0], function (z) {
        return f([z]); }];
    return x;
};
f;

