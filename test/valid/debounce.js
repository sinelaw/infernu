var $timeout = {
    set: function(f, num) { num = 0; return 0; },
    cancel: function(t) { t=0; }
};

function debounce(f, initial, millis) {
    var timer = 0;
    var res = initial;
    var timerSet = false;
    var scheduledArgs = [];
    function resetTimeout() {
        if (timerSet) {
            $timeout.cancel(timer);
        }
        timerSet = true;
        timer = $timeout.set(function() {
            // Note: f may end up running more than once per args set.
            res = f(scheduledArgs[0]);//.apply(null, scheduledArgs);
        }, millis);
        return res;
    }
    return function(arg) {
        scheduledArgs = [arg]; //Array.prototype.slice.call(arguments, 0);
        return {
            invoke: function() {
                return resetTimeout();
            }
        };
    };
}
var fonce = debounce(function (x) {  x = 0; return 'a'; }, 'c', 3);
var y = fonce(2).invoke();
y;
//return debounce;
