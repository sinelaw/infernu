function map(arr, f) {
    var i = 0;
    var res = [];
    for (i = 0; i < arr.length; i = i + 1) {
        res.push(f(arr[i]));
    }
    return res;
}


var arnum = map([1,2,3], function (x) { return x + 1; });

var arstr = map(['a','b','c'], function (x) { return x; });

arnum;
arstr;
