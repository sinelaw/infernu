function Promise() {
    var thens = [];
    var that = this;
    that.then = function(cb) {
        var p = new Promise();
        thens.push(function(val) { p.resolve(cb(val)); });
        return p;
    };
    that.resolve = function(val) {
        var i = 0;
        for (i = 0; i < thens.length; i++) {
            thens[i](val);
        }
        that.then = function(cb) {
            var p = new Promise();
            p.resolve(cb(val));
            return p;
        };
    };
}; 


