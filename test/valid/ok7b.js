
function makeX(setX_) {
    var setX = function (v2) { setX_(v2); return true; };
    return setX;
}
var mx1 = makeX(false);
mx1(false);
var mx2 = makeX(false);
mx2(3);
