var o2 =  { 
    last: function() { 
        var y = []; 
        return function(x2) { 
            var z = y; 
            y = [x2]; 
            return z; 
        }; 
    }
};

o2;


var f = o2.last();

f('a');

f(3);



