/* @flow */
function foo(b) { if (b) { return 21; } else { return ''; } }
function bar(b) { // : number
  var x = foo(b);
  var y = foo(b);
  if (typeof x == 'number' && typeof y == 'number') { return x + y; }
  return 0;
}

function poly<X>(x : X) : X {
    if (typeof x == 'number') { return 0; }
    return x;
}