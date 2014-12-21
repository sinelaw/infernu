function f(x) {
    x.stuff = 2;
    return x;
}
var arr = [];
f(arr[0]);
arr[0].muff = 3;
arr[0] = { muff: 123 }; // should require also .stuff = <number> to work. 
return arr;
