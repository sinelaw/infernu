function Base() {
    this.x = 0;
}

function Sub() {
    Base.call(this);

    this.y = 'b';
}

var a = new Sub();
a.x = 2;
a.y = 'c';
