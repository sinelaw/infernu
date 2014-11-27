function f(x) {
    var len =  x.length;
    return { obj: x, len: len };
}
var y = f({ moshe: 3, length: 'a' });
var z = f({ length: 2 });

var g = y.len;
g = z.obj.length;
g;
