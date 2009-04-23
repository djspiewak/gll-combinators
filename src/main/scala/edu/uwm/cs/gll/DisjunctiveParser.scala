package edu.uwm.cs.gll

class DisjunctiveParser[A](l: =>Parser[A], r: =>Parser[A]) extends NonTerminalParser[A] with Thunkable {
  private lazy val left = l
  private lazy val right = r
  
  private lazy val leftClass = thunk[Parser[A]]('l).getClass
  private lazy val rightClass = thunk[Parser[A]]('r).getClass
  
  lazy val gather = gatherImpl(Set())

  /**
   * The PREDICT table for this disjunction.  Please note that
   * this is a very different concept from the standard LL(k)
   * PREDICT set.  Specifically, the PREDICT table allows for
   * ambiguity in the prediction while still retaining <i>O(1)</i>
   * dispatch on disjunctions which are LL(1) and <i>near-O(1)</i>
   * for disjunctions which are not.
   *
   * Note that this is not actually sufficient to handle all
   * CFGs allowed by GLL.  Specifically, parsers with an empty
   * FIRST set must be handled specially.
   */
  lazy val predict = {
    gather.foldLeft(Map[Char, Set[Parser[A]]]()) { (map, p) =>
      p.first.foldLeft(map) { (map, c) =>
        if (map contains c)
          map(c) += p
        else
          map + (c -> Set(p))
      }
    }
  }

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
        set <- predict get in.head
        p <- set
      } p.queue(t, in)(f)
    } else {
      val thunk = new ThunkParser(this) {
        def queue(t: Trampoline, in: Stream[Char])(f: (A, Stream[Char])=>Unit) {
          var results = Set[(Any, Stream[Char])]()    // merge results
          
          for {
            pre <- gather
            val p = pre.asInstanceOf[Parser[A]]
            
            // [(S = {}) -> (FIRST = {})] /\ [~(S = {}) -> (S[0] \in FIRST \/ FIRST = {})]
            if (!in.isEmpty || p.first.size == 0) &&
              (in.isEmpty || (p.first.contains(in.head) || p.first.size == 0))      // lookahead
          } t.push(p, in) { (v, tail) =>
            val tuple = (v, tail)

            if (!results.contains(tuple)) {
              println("Disjunctive reduce: " + tuple)

              f(v.asInstanceOf[A], tail)
              results += tuple
            }
          }
        }
      }

      t.push(thunk, in)(f.asInstanceOf[(Any, Stream[Char])=>Unit])
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
      this.leftClass == that.leftClass && this.rightClass == that.rightClass
    }
    
    case _ => false
  }
  
  override def hashCode = leftClass.hashCode + rightClass.hashCode
  
  override def toString = {
    val left = leftClass.getName
    val right = rightClass.getName
    "(%s | %s)".format(left.substring(left.length - 3), right.substring(right.length - 3))
  }
}
