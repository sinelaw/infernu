function Base() {
    this.x = 0;
}

function Sub() {
    this.y = 'b';
}

var a = new Sub();
a.x = 2;
a.y = 'c';
