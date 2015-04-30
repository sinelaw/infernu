function Promise() {
    var thens = [];
    var that = this;
    function applyResolution(cb, promise, val) {
        return promise.resolve(cb(val));
    }
    this.then = function(cb) {
        var promise = new Promise() ;
        thens.push({ cb: cb, promise: promise});
        return promise;
    };
    this.resolve = function(val) {
        var i = 0;
        for (i = 0; i < thens.length; i++) {
//            applyResolution(thens[i].cb, thens[i].promise, val);
        }
        that.then = function(cb) {
            var promise = new Promise();
  //          applyResolution(cb, promise, val);
            return promise;
        };
    };
}; 
