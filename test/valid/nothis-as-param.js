
function id(x) {
    return x;
}
function test(f) {
    var obj = { f: f };

    obj.f(2);

    var g = obj.f;

    g(3);
}

test(id);
