var f = function (x) {
    return function (y) {
        return [x,y];
    };
};
function g(x, y) {
    return [x, y];
};
var z = [f(0), g(0)];
z;
