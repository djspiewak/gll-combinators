package edu.uwm.cs.gll

import scala.collection.mutable.{Buffer, ListBuffer}
import Global._

class DisjunctiveParser[A](l: =>Parser[A], r: =>Parser[A]) extends NonTerminalParser[A] with Thunkable {
  private lazy val left = l
  private lazy val right = r
  
  private lazy val leftClass = thunk[Parser[A]]('l).getClass
  private lazy val rightClass = thunk[Parser[A]]('r).getClass
  
  lazy val gather = gatherImpl(Set()).toList

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
    if (seen contains this)
      None          // left-recursion detected!
    else {
      val newSeen = seen + this
      
      val leftFirst = left.computeFirst(newSeen) getOrElse Set()
      val rightFirst = right.computeFirst(newSeen) getOrElse Set()
      
      if (leftFirst == UniversalCharSet || rightFirst == UniversalCharSet)
        Some(UniversalCharSet)
      else
        Some(leftFirst ++ rightFirst)
    }
  }
  
  def queue(t: Trampoline, in: Stream[Char])(f: Result[A]=>Unit) {
    if (isLL1) {        // graceful degrade to LL(1)
      trace("Detected LL(1): " + this)
      
      for {
        set <- predict get in.head
        p <- set
      } p.queue(t, in)(f)
    } else {
      val thunk = new ThunkParser(this) {
        def queue(t: Trampoline, in: Stream[Char])(f: Result[A]=>Unit) {
          var results = Set[Result[A]]()    // merge results
          
          for {
            pre <- gather
            val p = pre.asInstanceOf[Parser[A]]
            
            // [(S = {}) -> (FIRST = {})] /\ [~(S = {}) -> (S[0] \in FIRST \/ FIRST = {})]
            if (!in.isEmpty || p.first.size == 0) &&
              (in.isEmpty || (p.first.contains(in.head) || p.first.size == 0))      // lookahead
          } t.push(p, in) { res =>
            if (!results.contains(res)) {
              tracef("Reduced: %s *=> %s%n".format(this, res))

              f(res)
              results += res
            }
          }
        }
      }

      t.push(thunk, in)(f)
    }
  }
  
  private def gatherImpl(seen: Set[DisjunctiveParser[A]]): Buffer[Parser[A]] = {
    val newSeen = seen + this
    
    def process(p: Parser[A]) = p match {
      case d: DisjunctiveParser[A] => {
        if (!seen.contains(d))
          d.gatherImpl(newSeen)
        else
          new ListBuffer[Parser[A]]
      }
      
      case p => p +: new ListBuffer[Parser[A]]
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
    val tail = """.*\$([^\$]+\$\d+)$"""r
    
    val tail(left) = leftClass.getName
    val tail(right) = rightClass.getName
    
    "(%s | %s)".format(left, right)
  }
}
