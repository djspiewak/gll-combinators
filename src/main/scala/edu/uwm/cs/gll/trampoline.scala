package edu.uwm.cs.gll

import scala.collection.mutable.{Queue, Set}
import scala.actors.Actor

class Trampoline { outer =>
  private type Potential = (Parser[R], Stream[Char], (R, Stream[Char])=>Unit) forSome { type R }
  
  private val queue = new Queue[Potential]
  private val set = new mutable.Set[Potential]
  private val popped = new mutable.Set[Potential]   // prevents later convergence
  
  def run() {
    while (!queue.empty) {
      val (p, s, f) = pop()
      p.queue(this)(s, f)
    }
  }
  
  def push[R](p: Parser[R], s: Stream[Char])(f: (R, Stream[Char])=Unit) {
    val setTuple = (p, s)
    if (!set.contains(setTuple) && !popped.contains(setTuple)) {
      queue += (p, s, f)
      set += setTuple
    }
  }
  
  private def pop() = {
    val back @ (p, s, _) = queue.dequeue()
    set -= (p, s)
    popped += (p, s)
    
    back
  }
}

