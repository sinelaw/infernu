function Promise () {
    var that = this;
    this.then = function (rej) { };
    this.reject = function (ex) {
        that.then = function(rej) { rej(ex); };
    };

}; 

