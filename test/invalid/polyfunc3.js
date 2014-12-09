function debounce(f) {
    var timer = 0;
    var timerSet = false;
    var scheduledArgs = [];
    function resetTimeout() {
        if (timerSet) {
        }
        timerSet = true;
        function g() {
            // Note: f may end up running more than once per args set.
            f(scheduledArgs[0]);//.apply(null, scheduledArgs);
        }
        return g;
    }
    return function(arg) {
        scheduledArgs = [arg]; //Array.prototype.slice.call(arguments, 0);
        return {
            invoke: function() {
                resetTimeout();
            }
        };
    };
}
var fonce = debounce(function (x) {  return x[0]; }, 0);
fonce('a');
//return debounce;
