function makeX(_) {
    var x = [];
    var setX = function (v2, v3) { x = [v2]; return v3; };
    return setX;
}
var mx1 = makeX(false);
mx1(false, 1);
var mx2 = makeX(false);
mx2(1, 'bla');
mx1;
mx2;
