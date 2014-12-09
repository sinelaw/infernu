function makeX() {
    var x = [];
    var setX = function (v2) { x = [v2]; return true; };
    return { setX: setX, getX: function() { return x; } };
}
var mx1 = makeX();
mx1.setX(false);
var x1 = mx1.getX();
var mx2 = makeX();
mx2.setX(3);
var x2 = mx2.getX();
