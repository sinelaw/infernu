function magnitude(p) {
    var a = p.x;
    var b = p.y;//(p.x*p.x) + (p.y*p.y);
    return [a,b];
}

magnitude({ x: 1, y: 2 });
magnitude({ x: false, y: true });
