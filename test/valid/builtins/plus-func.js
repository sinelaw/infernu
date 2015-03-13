// let _/_ = undefined
//  in let f = (\(this, x, y) -> let return = []
//          in return[0.0] := (+ (null, x, y));          // + :: forall b. Plus b => b -> b -> b    instantiated here -> Plus c => c -> c -> c
//             return[0.0])
//      in let _/_ = f
//          in _/_ 
function f(x,y) { return x + y; }


f;


f(1,2);


f('b','c');
