var obj = { bla: function(x) {return x; } };
obj = { bla: function(x) {return 2; } }; 
var x = 'a';
x = obj.bla('b'); // returns 2, incompatible with 'a'

