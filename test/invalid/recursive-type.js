var getObj = function (x) {
    return {
        x: x,
        method: function(y) { this.x = y;  return this; }
    };
};
var x = getObj(2).method(3);
var y = getObj('a').method('a');
var z = x == y; // should fail. checker should enforce weak equality to only be applied when both sides have the same type.

