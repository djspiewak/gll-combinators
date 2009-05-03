val average = 
fn weight => fn list =>  
let val iterator = fn (x,(sum,length)) => (sum + weight x, length + 1) 
    val (sum,length) = foldl iterator (0,0) list 
in sum div length end

val find_best =
fn weight => fn lists =>
let val average = average weight
    val iterator = fn (list,(best,max)) => 
                     let val avg_list = average list
                     in if avg_list > max then
                          (list,avg_list)
                        else 
                          (best,max)
                     end
    val (best,_) = foldl iterator (nil,0) lists
in best end

val find_best_simple = find_best 1

(* Here is a typical error report, for comparison:

ex1.sml:20.1-20.35 Error: operator and operand don't agree [literal]
  operator domain: 'Z -> int
  operand:         int
  in expression:
    find_best 1

*)
