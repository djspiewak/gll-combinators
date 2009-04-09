package edu.uwm.cs.gll

import scala.collection.mutable.{Queue, Set}
import scala.actors.Actor

class Trampoline extends Actor {
  private implicit val default = this
  
  start()
  
  def act() {
    loop {
      react {
        case Push(p, in, cont) => 
      }
    }
  }
}

class DispatchSet extends Actor {
  type Potential = (Parser[R], Stream[Char], R=>()) forSome { type R }
  
  private val queue = new Queue[Potential]
  private val set = new mutable.Set[Potential]
  
  start()
  
  def act() {
    loop {
      case Push(p, in, cont) => {
        val tuple = (p, in, cont)
        
        if (!set.contains(tuple)) {
          queue += tuple
          set += tuple
        }
      }
      
      case Pop(target) => {
        val tuple = queue.dequeue
        set -= tuple
        
        target ! tuple
      }
    }
  }
}
