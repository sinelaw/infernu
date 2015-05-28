function map(arr, f) {
    var i = 0;
    var res = [];
    arr.forEach(function(x,i,arr2) {
        res.push(f(x));
    });
    return res;
}


var arnum = map([1,2,3], function (x) { return x + 1; });

//var arstr = map(['a','b','c'], function (x) { return x; });

//arnum;
//arstr;
