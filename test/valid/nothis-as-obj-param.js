
function id(x) {
    return x;
}
function test(obj) {
    obj.f(2);

    var g = obj.f;

    g(3);
}
