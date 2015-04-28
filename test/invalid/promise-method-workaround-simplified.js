// Based on https://gist.github.com/unscriptable/814052 - (c) copyright unscriptable.com / John Hann, License MIT

function Promise () {
    var _thens = [];
    var that = this;

    this.then = function (onResolve, onReject) {
        _thens.push({ resolve: onResolve, reject: onReject });
    };
    
    var resolve  = function (val) {
        var errorFunc = function(val) { throw new Error('Already completed.'); };
        var t3 = that;
        that.then = function(resolve, reject) { resolve(val); };
        that.resolve = errorFunc;
        var r = _thens[0].resolve;
        r(val);
        _thens = [];
    };

    var t1 = resolve;
    this.resolve = resolve;
    var t2 = this;
}; 

var promise = new Promise();

var str = 'a';

promise.then(function(x) {str =x; }, function (ex) { });

promise.resolve(3); // error; 3 is not a string

