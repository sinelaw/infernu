function (x) {  return x; }
function debounce(f) {
    var current = [];
    return function(arg) {
        var old = current.length > 0 ? current[0] : arg;
        current = [arg];
        return f(old);
    };
}
var fonce = debounce(function (x) {  return x[0]; });
var x = fonce([3]);
var fonce2 = debounce(function (x) {  return x[0]; });
var y = fonce2(['s']);
//x.length;
