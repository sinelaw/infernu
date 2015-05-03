var each = function(list, iteratee) {
    var i = 0;
    for (i = 0; i < list.length; i++) {
        iteratee(list[i], i, list);
    }
    return list;
};

var p = [];
each([1,2,3], function(e,i,l) { p.push(e + 55); });
// would give an obscure error:
//each([1,2,3], function(e,i,l) { p.push(e + 'ah'); });

//p;
