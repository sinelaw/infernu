var r = /123/g;

var m = r.exec('12');


var x = 0;
x = m.index;

var s = '';
var res = m[0];
if (typeof res !== 'undefined') {
    s = res;
}
s = m.input;

