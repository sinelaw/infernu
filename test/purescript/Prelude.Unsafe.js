/* global exports */
"use strict";

// module Prelude.Unsafe
function module(exports) {

exports.unsafeIndex = function(xs) {
  return function(n) {
    return xs[n];
  };
};
}
