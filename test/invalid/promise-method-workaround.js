// Based on https://gist.github.com/unscriptable/814052 - (c) copyright unscriptable.com / John Hann, License MIT

function _push(arr, val) { arr[0] = val; } // dummy
function len(arr) { return 0; }

function Promise () {
    var _thens = [];
    var that = this;
    /* This is the "front end" API. */
    
    // then(onResolve, onReject): Code waiting for this promise uses the
    // then() method to be notified when the promise is complete. There
    // are two completion callbacks: onReject and onResolve. A more
    // robust promise implementation will also have an onProgress handler.
    this.then = function (onResolve, onReject) {
        // capture calls to then()
        _push(_thens, { resolve: onResolve, reject: onReject });
    };
    
    /* This is the "back end" API. */
    
    // resolve(resolvedValue): The resolve() method is called when a promise
    // is resolved (duh). The resolved value (if any) is passed by the resolver
    // to this method. All waiting onResolve callbacks are called
    // and any future ones are, too, each being passed the resolved value.
    this.resolve = function (val) {
        var errorFunc = function(val) { throw new Error('Already completed.'); };
        var i = 0;
        that.then = function(resolve, reject) { resolve(val); };
        that.resolve = errorFunc;
        that.reject = errorFunc;
        for (i = 0; i < len(_thens); i++) {
            var r = _thens[i].resolve;
            r(val);
        }
        _thens = [];
    };
    
    // reject(exception): The reject() method is called when a promise cannot
    // be resolved. Typically, you'd pass an exception as the single parameter,
    // but any other argument, including none at all, is acceptable.
    // All waiting and all future onReject callbacks are called when reject()
    // is called and are passed the exception parameter.
    this.reject = function (ex) {
        var errorFunc = function(val) { throw new Error('Already completed.'); };
        var i = 0;
        that.then = function(resolve, reject) { reject(ex); };
        that.resolve = errorFunc;
        that.reject = errorFunc;
        for (i = 0; i < len(_thens); i++) {
            var r = _thens[i].reject;
            r(ex);
        }
        _thens = [];
    };

}; 

var promise = new Promise();

var str = 'a';

promise.then(function(x) {str =x; }, function (ex) { });

promise.resolve(3); // error; 3 is not a string

