package edu.uwm.cs.gll

class SequentialParser[+A, +B](left: Parser[A], right: Parser[B]) extends NonTerminalParser[~[A, B]] {
  def computeFirst(seen: Set[Parser[Any]]) = {
    if (seen contains this) Set()
    else {
      val newSeen = seen + this
      val lFirst = left.computeFirst(newSeen)
      
      if (lFirst == Set()) right.computeFirst(newSeen)
      else lFirst
    }
  }
  
  def queue(t: Trampoline, in: Stream[Char])(f: (~[A, B], Stream[Char])=>Unit) {
    left.queue(t, in) { (res1, tail) =>
      right.queue(t, tail) { (res2, tail) =>
        f(new ~(res1, res2), tail)
      }
    }
  }
  
  override def toString = "(%s ~ %s)".format(left, right)
}
