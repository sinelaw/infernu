var arr = [function(x) { return x; }];

arr.push(function(x) { return 0; });

arr[0](1);

var obj = { f: arr[0] };

obj.f(1);
