package edu.uwm.cs.gll

class SequentialParser[+A, +B](private val left: Parser[A], private val right: Parser[B]) extends NonTerminalParser[~[A, B]] {
  def computeFirst(seen: Set[Parser[Any]]) = {
    if (seen contains this) None    // left-recursion detected!
    else {
      val newSeen = seen + this
      val sub = left.computeFirst(newSeen)
      
      sub flatMap { set =>
        if (set.size == 0)
          right.computeFirst(newSeen)
        else
          sub
      }
    }
  }
  
  def queue(t: Trampoline, in: Stream[Char])(f: Result[~[A, B]]=>Unit) {
    left.queue(t, in) {
      case Success(res1, tail) => {
        right.queue(t, tail) {
          case Success(res2, tail) => f(Success(new ~(res1, res2), tail))
          
          case res: Failure => f(res)
        }
      }
      
      case res: Failure => f(res)
    }
  }
  
  override def equals(other: Any) = other match {
    case that: SequentialParser[A, B] => {
      this.left == that.left && this.right == that.right
    }
    
    case _ => false
  }
  
  override def hashCode = left.hashCode + right.hashCode

  override def toString = "(%s ~ %s)".format(left, right)
}
