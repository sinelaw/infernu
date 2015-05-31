function Foo() {
    this.bar = function() { return new Foo(); };
}

