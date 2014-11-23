function getId(_) { return function (x) { return x; }; }
var id1 = getId(false);
id1(2);
id1('a');
id1;
