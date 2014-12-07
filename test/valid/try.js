try {
}
catch (e) {
    e = 2;
}
finally {
}

var x = 2;
try {
    x = 3;
    throw Error("hi");
}
catch (bla) {
    x = bla;
}
finally {
    x = 3;
}
