package edu.uwm.cs.gll

class SequentialParser[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[~[A, B]] {
  def computeFirst(seen: Set[Parser[Any]]) = {
    if (seen contains this) Set()
    else {
      val lFirst = left.computeFirst(seen + this)
      
      if (lFirst == Set()) right.computeFirst(seen + this)
      else lFirst
    }
  }
  
  def queue(in: Stream[Char])(target: Actor)(implicit t: Trampoline) {
    def continue(res1: A, in: Stream[Char]) {
      def finish(res2: B, tail: Stream[Char]) {
        target ! Success(~(res1, res2), tail)
      }
      
      if (right.terminal) {
        right(in) match {
          case Success(res2, tail) => finish(res2, tail)
          case f => target ! f
        }
      } else {
        t ! Push(right, in)(finish)
      }
    }
    
    if (left.terminal) {
      left(in) match {
        case Success(res, tail) => continue(res, tail)
        case f => target ! f
      }
    } else {
      t ! Push(left, in)(continue)
    }
  }
}
