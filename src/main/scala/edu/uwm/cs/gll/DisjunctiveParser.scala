package edu.uwm.cs.gll

class DisjunctiveParser[+A](left: Parser[A], right: Parser[A]) extends NonTerminalParser[A] {
  lazy val gather = gatherImpl(Set())
  
  def queue(t: Trampoline, in: Stream[Char])(f: (A, Stream[Char])=>Unit) {
    for (p <- gather; if p.first contains in.head || p.first.size == 0) {     // lookahead
      p.queue(t, in)(f)
    }
  }
  
  private def gatherImpl(seen: Set[DisjunctiveParser[A]]): Set[Parser[A]] = {
    lazy val newSeen = seen + this
    
    def process(p: Parser[A]) = {
      case d: DisjunctiveParser[A] => {
        if (!seen.contains(d))
          d.gatherImpl(newSeen)
        else
          Set[Parser[A]]()
      }
      
      case p => Set(p)
    }
    
    process(left) ++ process(right)
  }
}
