function open(o) {
    o.x = 0;
    return o;
}
var y = open({x:0});
y.z = 3;
y;
