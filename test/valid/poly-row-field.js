var obj = {  magnitude:
             function(p) {
                 var a = p.x;
                 var b = p.y;//(p.x*p.x) + (p.y*p.y);
                 return [a,b];
             }
          };

obj.magnitude({ x: 1, y: 2 });
obj.magnitude({ x: false, y: true });

