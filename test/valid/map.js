function map(arr, f) {
    var i = 0;
    var res = [];
    for (i = 0; i < arr.length; i = i + 1) {
        res.push(f(arr[i]));
    }
    return res;
}
