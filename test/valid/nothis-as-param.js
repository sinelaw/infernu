function id(x) { return x; }

function test(f) {
    var obj = { f: f };
    obj.f(2);
    var g = obj.f;
    // error, because 'f' was already inferred to take a 'this' parameter with the same type as 'obj'
    g(3);
}

test(id);
