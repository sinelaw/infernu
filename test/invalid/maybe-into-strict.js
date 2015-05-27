function f(x, i) {
    return x[i];
}

var foo = 2;

// f should return ?number, but foo :: number. so should fail.
foo = f([1,2,3], 0);
