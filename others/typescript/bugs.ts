b = 3; // var hoisting
var a = 'a';
var b = +a; // implicit conversion to string
var c = !a; // implicit conversion to bool
delete a;   // delete works on anything, not just object properties
var d = void a; // why do we need void?
var e = this.foo; // 'this'' has same horrible behavior as in javascript (and is always in context)
