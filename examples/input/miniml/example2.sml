val mapActR =
fn f => fn (xs,state) =>  
let val iter = fn (x,(xs,state)) => 
               let val (x,state) = f (x,state)
               in (x :: xs, state) end
in foldr iter (nil,state) xs end

val mapActL =
fn f => fn (xs,state) =>  
let val iter = fn (x,(xs,state)) => 
               let val (x,state) = f (x,state)
               in (xs @ x, state) end
in foldl iter (nil,state) xs end  

val isEven = fn n => n div 2 = 0

val doubleOdds =
fn xs => 
(* multiplies each odd element of xs by 2,
   keeps track by how much the list sum gets incremented *)
let val f = fn (n,inc) => if isEven n then 
                            ( n, inc )
                          else 
                            ( 2 * n, inc + n)
in mapActL f (xs,0) end

(* Here is a typical error report, for comparison:

ex2.sml:21.1-25.24 Error: operator and operand don't agree [tycon mismatch]
  operator domain: int * int -> 'Z list * int
  operand:         int * int -> int * int
  in expression:
    mapActL f

*)


