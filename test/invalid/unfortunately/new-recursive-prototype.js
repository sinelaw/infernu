function Foo() {

}

Foo.prototype.bar = function() { return new Foo(); };


var x = new Foo();

var y = x.bar();
