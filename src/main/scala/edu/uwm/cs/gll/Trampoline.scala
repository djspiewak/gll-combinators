package edu.uwm.cs.gll

import scala.collection.mutable

class Trampoline {
  private type Potential = (Parser[Any], Stream[Char])
  
  private val queue = new mutable.Queue[Potential]
  private val backlinks = mutable.Map[Potential, mutable.Set[(Any, Stream[Char])=>Unit]]()
  private val set = mutable.Set[Potential]()
  private val popped = mutable.Set[Potential]()
  
  def run() {
    while (queue.length > 0) {
      val (p, s, f) = pop()
      p.queue(this, s)(f)
    }
  }
  
  def push[A](p: Parser[Any], s: Stream[Char])(f: (Any, Stream[Char])=>Unit) {
    val tuple = (p, s)
    
    if (popped.contains(tuple)) {
      // TODO need to cache result for late-comers
    } else {
      if (!set.contains(tuple)) {
        queue += tuple
        backlinks += (tuple -> mutable.Set(f))
        set += tuple
      } else {
        backlinks(tuple) += f
      }
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
        
    set -= tuple
    popped += tuple
    
    (p, s, f)
  }
}

