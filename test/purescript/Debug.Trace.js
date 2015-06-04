/* global exports */
"use strict";

// module Debug.Trace
function module(exports) {

exports.trace = function(s) {
  return function() {
    console.log(s);
    return {};
  };
};

}
