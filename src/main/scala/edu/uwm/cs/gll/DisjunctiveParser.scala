package edu.uwm.cs.gll

class DisjunctiveParser[A](l: =>Parser[A], r: =>Parser[A]) extends NonTerminalParser[A] with Thunkable {
  private lazy val left = l
  private lazy val right = r
  
  private lazy val leftClass = thunk[Parser[A]]('l).getClass
  private lazy val rightClass = thunk[Parser[A]]('r).getClass
  
  lazy val gather = gatherImpl(Set())

  /**
   * Checks if all FIRST sets are disjoint and none
   * are empty.  This is convergent even for
   * left-recursive parsers.
   */ 
  lazy val isLL1 = {
    val sets = gather map { _.first }
    val totalSize = sets.foldLeft(0) { _ + _.size }
    val intersect = sets.foldLeft(Set[Char]()) { _ ** _ }
    (totalSize == intersect.size) && (sets forall { _.size > 0 })
  }
  
  def computeFirst(seen: Set[Parser[Any]]) = {
    lazy val newSeen = seen + this
    
    if (seen contains this)
      Set()
    else
      left.computeFirst(newSeen) ++ right.computeFirst(newSeen)
  }
  
  def queue(t: Trampoline, in: Stream[Char])(f: (A, Stream[Char])=>Unit) {
    if (isLL1) {        // graceful degrade to LL(1)
      for {
	pre <- gather
	val p = pre.asInstanceOf[Parser[A]]
	
	if p.first.contains(in.head)     // lookahead
      } p.queue(t, in)(f)
    } else {
      var results = Set[(Any, Stream[Char])]()    // merge results
      
      for {
	pre <- gather
	val p = pre.asInstanceOf[Parser[A]]
	
	if (p.first.size == 0 && in.isEmpty) || 
          (p.first.size == 0 || (!in.isEmpty && p.first.contains(in.head)))     // lookahead
      } t.push(p, in) { (v, tail) =>
	val tuple = (v, tail)
	if (!results.contains(tuple)) {
          f(v.asInstanceOf[A], tail)
          results += tuple
        }
      }
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
  
  override def equals(other: Any) = other match {
    case that: DisjunctiveParser[A] => {
      this.leftClass == that.leftClass &&
        this.rightClass == that.rightClass
    }
    
    case _ => false
  }
  
  override def hashCode = leftClass.hashCode + rightClass.hashCode
  
  override def toString = "(%s | %s)".format(leftClass.getName, rightClass.getName)
}
