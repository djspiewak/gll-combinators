package edu.uwm.cs.gll

import scala.collection.mutable
import mutable.ListBuffer

import Global._

class Trampoline {
  private type Potential = (Parser[Any], Stream[Char])
  
  private val queue = new mutable.Queue[Potential]
  private val backlinks = mutable.Map[Potential, mutable.ListBuffer[(Any, Stream[Char])=>Unit]]()
  private val popped = mutable.Map[Potential, mutable.ListBuffer[(Any, Stream[Char])]]()
  
  def run() {
    while (queue.length > 0) {
      val (p, s, f) = pop()

      p.queue(this, s) { (v, s2) =>
        popped((p, s)) += ((v, s2))             // store the result for late-comers
        f(v, s2)

        trace("Saved: " + (p, s))
      }
    }
  }

  def push[A](p: Parser[Any], s: Stream[Char])(f: (Any, Stream[Char])=>Unit) {
    val tuple = (p, s)
    
    if (popped.contains(tuple) && popped(tuple).size > 0) {
      for ((v, s) <- popped(tuple)) {           // if we've already done that, use the result
        tracef("Revisited: %s *=> %s%n", tuple, (v, s))
        f(v, s)
      }
    } else {
      if (!backlinks.contains(tuple)) {
        queue += tuple
        backlinks += (tuple -> new ListBuffer[(Any, Stream[Char])=>Unit])
      }
      backlinks(tuple) += f

      trace("Pushed: " + tuple)
    }
  }
  
  private def pop() = {
    val tuple @ (p, s) = queue.dequeue()
    val f = {
      val links = backlinks(tuple)
      
      if (links.size == 1)
        links.toList.head
      else
        { (v: Any, tail: Stream[Char]) => links foreach { _(v, tail) } }
    }
    
    backlinks -= tuple
    popped += (tuple -> new ListBuffer[(Any, Stream[Char])])

    trace("Popped: " + tuple)
    
    (p, s, f)
  }
}

