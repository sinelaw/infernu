function Foo() {
    this.bar = function() { return new Foo(); };
}

var f = new Foo();

