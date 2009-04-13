package edu.uwm.cs.gll

import scala.collection.mutable

class Trampoline { outer =>
  private type Potential = (Parser[R], Stream[Char], (R, Stream[Char])=>Unit) forSome { type R }
  
  private val queue = new mutable.Queue[Potential]
  private val set = mutable.Set[(Parser[Any], Stream[Char])]()
  private val popped = mutable.Set[(Parser[Any], Stream[Char])]()   // prevents later convergence
  
  def run() {
    while (queue.length > 0) {
      val (p, s, f) = pop()
      p.queue(this, s)(f)
    }
  }
  
  def push[R](p: Parser[R], s: Stream[Char])(f: (R, Stream[Char])=>Unit) {
    val setTuple = (p, s)
    if (!set.contains(setTuple) && !popped.contains(setTuple)) {    // TODO is this right?
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

