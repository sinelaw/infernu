var obj =  { prop : function(x) { return x; } }; var barr =  [function(x) { return x; }]; obj.prop = barr[0]; barr[0](2); 
