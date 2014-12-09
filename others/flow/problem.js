/* @flow */
function debounce(f) {
    return function(arg) {
        return f(arg);
    };
}
var id = function (x) {  return x; };
var id1 = debounce(id);
var x = id1(3);
var id2 = debounce(id);
var y = id2('s');
//x.length;
