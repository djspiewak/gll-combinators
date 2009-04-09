package edu.uwm.cs.gll

sealed trait Parser[+R] extends (Stream[Char]=>Result[R]) {
  val terminal: Boolean
  
  lazy val first = computeFirst(Set())
  
  /**
   * @return The FIRST set for this parser, or the empty set
   *         if the production goes to \epsilon.
   */
  def computeFirst(s: Set[Parser[Any]]): Set[Char]
  
  // syntax
  
  def ~[R2](other: Parser[R2]): Parser[~[R, R2]] = new SequentialParser(this, other)
}

trait TerminalParser[+R] extends Parser[R] { self =>
  final val terminal = true
  
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
  final val terminal = false
}
