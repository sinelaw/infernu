function f(x) {
    return x;
}

var obj = { f: f };

var g = obj.f;

g(3);

obj.f(2);
