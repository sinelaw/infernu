function f(parent) {
    return parent.child;
}
var x = f({ child: { grandChild: false } });
x.grandChild;
