package edu.uwm.cs.gll

import scala.actors.Actor

sealed trait Parser[+R] extends (Stream[Char]=>Result[R]) {
  val terminal: Boolean
  
  lazy val first = computeFirst(Set())
  
  /**
   * @return The FIRST set for this parser, or the empty set
   *         if the production goes to \epsilon.
   */
  def computeFirst(s: Set[Parser[Any]]): Set[Char]
  
  def queue(in: Stream[Char])(target: Actor)(implicit t: Trampoline)
  
  // syntax
  
  def ~[R2](other: Parser[R2]): Parser[~[R, R2]] = new SequentialParser(this, other)
}

trait TerminalParser[+R] extends Parser[R] { self =>
  final val terminal = true
  
  /**
   * For non-terminal parsing, this just delegates back to apply()
   */
  def queue(in: Stream[Char])(target: Actor)(implicit t: Trampoline) {
    target ! apply(in)
  }
  
  def ~[R2](other: Parser[R2]) = if (other.terminal) {
    new TerminalParser[~[R, R2]] {
      def computeFirst(s: Set[Parser[Any]]) = self.computeFirst(s) match {
        case Set() => other.computeFirst(s)
        case s => s
      }
      
      def apply(in: Stream[Char]) = self(in) match {
        case Success(res1, tail) => other(in) match {
          case Success(res2, tail) => Success(~(res1, res2), tail)
          case f => f
        }
        
        case f => f
      }
    }
  } else super ~ other
}

trait NonTerminalParser[+R] extends Parser[R] {
  import Actor._
  
  final val terminal = false
  
  /**
   * This method takes care of kicking off a <i>new</i>
   * parse process.  We will never call this method to
   * handle a sub-parse.  In such situations, we will use
   * the trampoline to queue results.
   */
  def apply(in: Stream[Char]) = {
    val t = new Trampoline(self)
    t ! Push(this, in) { _ => () }
    
    receive { 
      case r: Result[R] => {
        t ! Dispose
        r
      }
    }
  }
}
