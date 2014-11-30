/* @flow */
var y = 2;
y = 'hi';

var r = y + 2;

var l = r.length;


// flow allows all of the above without errors or warnings.
// r will be 'hi2' and l will be equal to 3.

