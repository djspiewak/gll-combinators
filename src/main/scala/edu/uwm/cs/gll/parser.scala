package edu.uwm.cs.gll

sealed trait Parser[+R] extends (Stream[Char]=>Set[Result[R]]) {
  val terminal: Boolean
  
  lazy val first = computeFirst(Set())
  
  /**
   * @return The FIRST set for this parser, or the empty set
   *         if the production goes to \epsilon.
   */
  def computeFirst(s: Set[Parser[Any]]): Set[Char]
  
  // TODO handle failure somehow
  def queue(t: Trampoline, in: Stream[Char])(f: (R, Stream[Char])=>Unit)
  
  // syntax
  
  def ~[R2](other: Parser[R2]): Parser[~[R, R2]] = new SequentialParser(this, other)
}

trait TerminalParser[+R] extends Parser[R] { self =>
  final val terminal = true
  
  /**
   * For terminal parsing, this just delegates back to apply()
   */
  def queue(t: Trampoline, in: Stream[Char])(f: (R, Stream[Char])=>Unit) {
    apply(in) foreach {
      case Success(v, tail) => f(v, tail)
      case _ => ()
    }
  }
  
  def ~[R2](other: Parser[R2]) = if (other.terminal) {
    new TerminalParser[~[R, R2]] {
      def computeFirst(s: Set[Parser[Any]]) = {
        val sub = self.computeFirst(s)
        
        if (sub.size == 0)
          other.computeFirst(s)
        else
          sub
      }
      
      def apply(in: Stream[Char]) = self(in) flatMap {
        case Success(res1, tail) => other(in) map {
          case  Success(res2, tail) => Success(new ~(res1, res2), tail)
          case f: Failure => f
        }
        
        case f: Failure => Set(f)
      }
    }
  } else super.~(other)
}

trait NonTerminalParser[+R] extends Parser[R] {
  final val terminal = false
  
  /**
   * This method takes care of kicking off a <i>new</i>
   * parse process.  We will never call this method to
   * handle a sub-parse.  In such situations, we will use
   * the trampoline to queue results.
   */
  def apply(in: Stream[Char]) = {
    val t = new Trampoline
    
    var back = Set[Result[R]]()
    queue(t, in) { (res, tail) => back += Success(res, tail) }
    t.run()
    
    if (back.size == 0) back += Failure("No valid derivations", in)
    
    back
  }
}
