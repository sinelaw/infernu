function zzz() { return 0; }

function test(y) {
    function callback() {
        return y;
    }
    var x= {
        invoke: function() {
            return callback();
        }
    };
    
}


test('hi');
