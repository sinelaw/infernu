function f1(x) {  return x; }
function debounce(f) {
    var current = [];
    var has = false;
    return function(arg) {
        var old = has ? current[0] : arg;
	has = true;
        current = [arg];
        return f(old);
    };
}
var fonce = debounce(function (x) {  return x[0]; });
var x = fonce([3]);
var fonce2 = debounce(function (x) {  return x[0]; });
var y = fonce2(['s']);
//x.length;
