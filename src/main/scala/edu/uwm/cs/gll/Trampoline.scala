package edu.uwm.cs.gll

import scala.collection.mutable
import mutable.ListBuffer

import Global._

class Trampoline {
  private type Potential = (Parser[Any], Stream[Char])
  
  private val queue = new mutable.Queue[Potential]
  private val backlinks = mutable.Map[Potential, mutable.ListBuffer[Result[Any]=>Unit]]()
  private val popped = mutable.Map[Potential, mutable.ListBuffer[Result[Any]]]()
  
  def run() {
    while (queue.length > 0) {
      val (p, s, f) = pop()
      
      p.queue(this, s) { res =>
        popped((p, s)) += res             // store the result for late-comers
        f(res)
        
        tracef("Saved: %s *=> %s%n", (p, s), res)
      }
    }
  }

  def push[A](p: Parser[A], s: Stream[Char])(f: Result[A]=>Unit) {
    val tuple = (p, s)
    
    if (popped.contains(tuple) && popped(tuple).size > 0) {
      for (res <- popped(tuple)) {           // if we've already done that, use the result
        tracef("Revisited: %s *=> %s%n", tuple, res)
        f(res.asInstanceOf[Result[A]])
      }
    } else {
      if (!backlinks.contains(tuple)) {
        queue += tuple
        backlinks += (tuple -> new ListBuffer[Result[Any]=>Unit])
      }
      backlinks(tuple) += f.asInstanceOf[Result[Any]=>Unit]

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
        { res: Result[Any] => links foreach { _(res) } }
    }
    
    backlinks -= tuple
    popped += (tuple -> new ListBuffer[Result[Any]])

    trace("Popped: " + tuple)
    
    (p, s, f)
  }
}

