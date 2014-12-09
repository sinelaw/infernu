function makeX() {
    var x = [];
    var setX = function (v2) { x = [v2]; return true; };
    return setX;
}
var mx1 = makeX();
mx1(false);
var mx2 = makeX();
mx2(3);
