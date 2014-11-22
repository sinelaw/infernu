function makeX(_) {
    var x = [];
    var setX = function (v2) { x = [v2]; return true; };
    return setX;
}
var mx1 = makeX(0);
mx1(0);
mx1(false);
var mx2 = makeX(false);
mx2(false);
//makeX;
