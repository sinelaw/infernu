
function setFirst(arr, val) { arr[0] = val; }

function DoStuff() {
    var elems = [];
    this.set = function(y) { setFirst(elems, y); };
}
    
var x = new DoStuff();
x.set(2);
var y = new DoStuff();
y.set('a');
