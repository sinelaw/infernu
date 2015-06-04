/* global exports */
"use strict";

function Error(x) { this.name = ''; this.message = x; }
var console = { log: function(s) { } };

// module Assert
function module(exports) {

exports.error = function(msg) {
  throw msg;
};

exports.assertPartial = function(f) {
  return function() {
    try {
      return f();
    } catch (e) {
      if (e instanceof Error) return;
      throw new Error('Pattern match failure is not Error');
    }
  };
};

}
