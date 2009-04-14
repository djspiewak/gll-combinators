package edu.uwm.cs.gll

class DisjunctiveParser[A](left: Parser[A], right: Parser[A]) extends NonTerminalParser[A] {
  lazy val gather = gatherImpl(Set())
  
  def computeFirst(seen: Set[Parser[Any]]) = {
    if (seen contains this)
      Set()
    else
      left.computeFirst(seen + this) ++ right.computeFirst(seen + this)
  }
  
  def queue(t: Trampoline, in: Stream[Char])(f: (A, Stream[Char])=>Unit) {
    var results = Set[(A, Stream[Char])]()    // merge results
    val emptyStream = in.lengthCompare(0) == 0
    
    for {
      pre <- gather
      val p = pre.asInstanceOf[Parser[A]]
      
      if (emptyStream || p.first.contains(in.head)) || p.first.size == 0     // lookahead
    } p.queue(t, in) { (v, tail) => results += ((v, tail)) }
    
    for ((v, tail) <- results) {
      f(v, tail)
    }
  }
  
  private def gatherImpl(seen: Set[DisjunctiveParser[A]]): Set[Parser[A]] = {
    lazy val newSeen = seen + this
    
    def process(p: Parser[A]): Set[Parser[A]] = p match {
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
