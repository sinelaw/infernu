var f = function (x) {
    x.y = function (z) { return f({y:z}); };
    return x;
};

f({ y : function (x){
    var z = 2;
    z = x;
    return x;}
  }).y(0);
