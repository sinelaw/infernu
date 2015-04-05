var x = { ap: function(f) { f(); } };

var y = [];
x.ap(function() {y.push('a');});
x.ap(function() {y.push(3);});
