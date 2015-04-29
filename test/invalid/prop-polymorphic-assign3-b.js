~js var obj =  { prop : function(x) { return x; } }; var barr =  [function(x) { return x; }]; barr[0](2); obj.prop = barr[0];
