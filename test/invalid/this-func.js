function f(x) {
    this.doStuff(x);
    return x;
}
var bla={ f:f, doStuff: 789};
bla.f(2); // should fail, doStuff isn't a function.
