package edu.uwm.cs.gll

class DisjunctiveParser[A](l: =>Parser[A], r: =>Parser[A]) extends NonTerminalParser[A] {
  private lazy val left = l
  private lazy val right = r
  
  lazy val gather = gatherImpl(Set())
  
  def computeFirst(seen: Set[Parser[Any]]) = {
    lazy val newSeen = seen + this
    
    if (seen contains this)
      Set()
    else
      left.computeFirst(newSeen) ++ right.computeFirst(newSeen)
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
