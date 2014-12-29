//<ski>   # let rotate angle p = (p#new_x (cos angle *. p#get_x -. sin angle *. p#get_y))#new_y (sin angle *. p#get_x +. cos angle *. p#get_y);;
function cos(x) { return x; }
function sin(x) { return x; }
function rotate(angle, p) {
    p.new_x(cos(angle) * p.get_x() - sin(angle) * p.get_y());
    p.new_y(sin(angle) * p.get_x() + cos(angle) * p.get_y());
}
rotate;
