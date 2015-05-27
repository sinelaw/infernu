function f(x) {
  if (x != undefined) {
    // do somethin'
    // should be 'dead code' and could be eliminated
  }
}

f(3); // bug!
