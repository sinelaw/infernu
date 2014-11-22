function makeX(_) {
    var x = [];
    var setX = function (v2) { x = [v2]; return true; };
    return setX;
}
var mx1 = makeX(false);
mx1(false);
mx1(2);
mx1;
makeX;
