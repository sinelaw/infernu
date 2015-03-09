function debounce(f) {
    var scheduledArgs = [];
    return function(arg) {
        scheduledArgs = [arg];
        return {
            invoke: function() {
                f(scheduledArgs[0]);
            }
        };
    };
}
var fonce = debounce(function (x) {  return x[0]; });
fonce(false);
//return debounce;
