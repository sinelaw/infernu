function f(x) {
    var len =  x.length;
    return { obj: x, len: [len, 2] };
}
var y = f({ moshe: 3, length: 'a' });
//var z = f({ length: 'a' });
f;
y;
