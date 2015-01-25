function Promise() {
    var _thens = [];
    _thens = [];
    this.push = function(x) {
        _thens.push(x);
    };
}; 

var p = new Promise();
var p1 = p.push(1);

var pb = new Promise();
var pb1 = pb.push('2');


