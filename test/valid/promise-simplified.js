function _push(arr, val) { arr = val; return null; }

function Promise(x) {
    var _thens = x;
    _push(_thens, x);
    _thens = x;
}; 

var p = Promise(123);
var pb = Promise('a');


