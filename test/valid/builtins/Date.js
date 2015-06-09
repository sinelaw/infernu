var x = new Date();
var y = Date.now();
var z = Date.parse('2015-01-01');
var u = Date.UTC(2015,3,1,11,10,9,932);


var n = 1;

n = x.getDate();
n = x.getDay();
n = x.getFullYear();
n = x.getHours();
n = x.getMilliseconds();
n = x.getMinutes();
n = x.getMonth();
n = x.getSeconds();
n = x.getTime();
n = x.getTimezoneOffset();
n = x.getUTCDate();
n = x.getUTCDay();
n = x.getUTCFullYear();
n = x.getUTCHours();
n = x.getUTCMilliseconds();
n = x.getUTCMinutes();
n = x.getUTCMonth();
n = x.getUTCSeconds();
n = x.valueOf();

x.setDate(n);
x.setFullYear(n);
x.setHours(n);
x.setMilliseconds(n);
x.setMinutes(n);
x.setMonth(n);
x.setSeconds(n);
x.setTime(n);
x.setUTCDate(n);
x.setUTCFullYear(n);
x.setUTCHours(n);
x.setUTCMilliseconds(n);
x.setUTCMinutes(n);
x.setUTCMonth(n);
x.setUTCSeconds(n);
//x.setYear(n);

var s = '';
s = x.toDateString();
s = x.toISOString();
s = x.toJSON();
s = x.toString();
s = x.toTimeString();
s = x.toUTCString();

