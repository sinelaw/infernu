function f(x) { return x()(); };
f(function(){return function(){return 0; }; });
