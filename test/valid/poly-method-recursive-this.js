function foo(x, y) { return { o: x, v: x.bla(y) }; }

var obj = { bla: function(z) { return z == this.val; }, 
            val: false
          };


var detachedBla = obj.bla;
var otherObj = { val: 3, bloop: detachedBla };
otherObj.bloop(2);

// the returned obj2 is the same object as obj, but has a different type!
var obj2 = foo(obj, true).o;

var detachedBla2 = obj2.bla;
var otherObj2 = { val: 3, bloop: detachedBla2 };
// This fails becase otherObj2 is monomorphic. Disable the next line and compare the resulting type of otherObj2 with that of otherObj.
otherObj2.bloop(2);
