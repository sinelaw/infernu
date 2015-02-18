//<ski>   # let rotate angle p = (p#new_x (cos angle *. p#get_x -. sin angle *. p#get_y))#new_y (sin angle *. p#get_x +. cos angle *. p#get_y);;
function cos(x) { return x; }
function sin(x) { return x; }
function rotate(angle, p) {
    p.new_x(cos(angle) * p.get_x() - sin(angle) * p.get_y());
    p.new_y(sin(angle) * p.get_x() + cos(angle) * p.get_y());
}

function Point(x0, y0) {
    var x = x0, y = y0;
    this.new_x = function(x1) { x = x1; };
    this.new_y = function(y1) { y = y1; };
    this.get_x = function() { return x; };
    this.get_y = function() { return y; };
}

var p = new Point(1,2);

rotate(20, p);


