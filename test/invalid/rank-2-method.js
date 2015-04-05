// node doesn't like _
var _u = {
    each: function(list, iteratee) {
        var i = 0;
        for (i = 0; i < list.length; i++) {
            iteratee(list[i], i, list);
        }
        return list;
    }
};



var p = [];

function test(f) {
    _u.each([1,2,3], f);
}


_u.each([1,2,3], function(e,i,l) { p.push(e + 'aba'); });

p;
