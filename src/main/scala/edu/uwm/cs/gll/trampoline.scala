package edu.uwm.cs.gll

import scala.collection.mutable.{Queue, Set}
import scala.actors.Actor

class Trampoline(target: Actor) extends Actor { outer =>
  private type Potential = (Parser[R], Stream[Char], (R, Stream[Char])=>()) forSome { type R }
  
  private val queue = new Queue[Potential]
  private val set = new mutable.Set[Potential]
  
  start()
  
  def act() {
    react {
      case Push(p, in)(cont) => {
        val tuple = (p, in, cont)
        
        if (!set.contains(tuple)) {
          queue += tuple
          set += tuple
          
          new DispatchActor
        }
        
        act()
      }
      
      case Pop(target) => {
        val tuple = queue.dequeue
        set -= tuple
        
        target ! tuple
        
        act()
      }
      
      case Dispose => ()
    }
  }
}

