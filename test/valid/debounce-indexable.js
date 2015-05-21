var $timeout = {
    set: function(f, num) { num = 0; return 0; },
    cancel: function(t) { t=0; }
};

function debounce(f, millis) {
    var timer = 0;
    var timerSet = false;
    var scheduledArgs;
    function resetTimeout() {
        if (timerSet) {
            $timeout.cancel(timer);
        }
        timerSet = true;
        timer = $timeout.set(function() {
            // Note: f may end up running more than once per args set.
            if (typeof scheduledArgs !== 'undefined') {
                f(scheduledArgs);//.apply(null, scheduledArgs);
            } else {
                throw new Error("Assetion failed: timeout was called somehow before invocation!");
            }
        }, millis);
        return;
    }
    return function(arg) {
        scheduledArgs = arg; //Array.prototype.slice.call(arguments, 0);
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
