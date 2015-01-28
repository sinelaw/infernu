var obj = {  magnitude:
             function(p) {
                 var a = p.x;
                 var b = p.y;//(p.x*p.x) + (p.y*p.y);
                 return [a,b];
             }
          };

var m1 = obj.magnitude;
m1({ x: 1, y: 2 });
m1({ x: false, y: true });

