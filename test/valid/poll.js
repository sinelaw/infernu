function Date() {

}
var window = {
    setTimeout: function(f, t) {
        while (t > 0) {
            t -= 1;
        }
        f();
    }
};
function Error(s) { this.message = s; }

// Copied from http://davidwalsh.name/essential-javascript-functions
function poll(fn, callback, errback, timeout, interval) {
    var endTime = Number(new Date()) + (timeout || 2000);
    interval = interval || 100;

    (function p() {
            // If the condition is met, we're done!
            if(fn()) {
                callback();
            }
            // If the condition isn't met but the timeout hasn't elapsed, go again
            else if (Number(new Date()) < endTime) {
                window.setTimeout(p, interval);
            }
            // Didn't match and too much time, reject!
            else {
                errback(new Error('timed out'));
            }
    })();
}
