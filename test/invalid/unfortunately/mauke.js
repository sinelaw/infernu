var a = "x"; 
function foo(x) { 
    var e;
    if (false) { e = "equal"; }
    else { e = "not equal"; }
    return x;
} 
foo("x"); 
a = 42; 
foo(0);
