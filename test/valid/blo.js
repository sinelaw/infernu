function f(x) {
    var len =  x.length;
    return { obj: x, len: len };
}
var y = f({ moshe: 3, length: 4 });
var z = f({ length: 'a' });
f;

