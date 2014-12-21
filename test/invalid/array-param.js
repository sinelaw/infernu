function f(x) {
    x.stuff = 2;
    return x;
}
var arr = [];
arr[0] = f(arr[0]);
arr[0].muff = 3;
return arr;
