function Promise(x) {
    var _thens = [];
    _thens.push(x);
}; 

var p = Promise(123);
var pb = Promise('a');


