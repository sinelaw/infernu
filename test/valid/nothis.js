function f(x) {
    return x;
}

var obj = { f: f };

obj.f(2);

obj;

var g = obj.f;

g(3);
