function h(f) {
	 var g = function(x) {
	 	f(x(x));
         };
	 return g(g);
}
