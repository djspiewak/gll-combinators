package edu.uwm.cs.gll

import scala.collection.mutable
import mutable.{Buffer, ListBuffer}

import Global._

// I hate the way this file is organized, but I don't have a choice
trait Parsers {
  private val TAIL_ERROR_PATTERN = "Unexpected trailing characters: '%s'"
  
  implicit def literal(str: String): Parser[String] = new LiteralParser(str)
  
  def opt[A](p: Parser[A]) = p?
  
  def rep[A](p: Parser[A]) = p*
  
  def rep1[A](p: Parser[A]) = p+
  
  protected def processTail(tail: Stream[Char]) = if (tail.isEmpty) Some(tail) else None
  
  //////////////////////////////////////////////////////////////////////////////
  
  sealed trait Parser[+R] extends (Stream[Char]=>List[Result[R]]) { self =>
    val terminal: Boolean
    
    lazy val first = computeFirst(Set()) getOrElse Set()
    
    /**
     * @return The FIRST set for this parser, or the empty set
     *         if the production goes to \epsilon.
     */
    def computeFirst(seen: Set[Parser[Any]]): Option[Set[Char]]
    
    def queue(t: Trampoline, in: Stream[Char])(f: Result[R]=>Unit)
    
    // syntax
    
    def map[R2](f: R=>R2): Parser[R2]
    
    // operators
    
    def ~[R2](that: Parser[R2]): Parser[~[R, R2]] = new SequentialParser(this, that)
    
    def <~[R2](that: Parser[R2]) = this ~ that map { case a ~ _ => a }
    
    def ~>[R2](that: Parser[R2]) = this ~ that map { case _ ~ b => b }
    
    def * = (this+?) map { _ getOrElse Nil }
    
    def +(): Parser[List[R]] = new NonTerminalParser[List[R]] {
      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen + this)
      
      def queue(t: Trampoline, in: Stream[Char])(f: Result[List[R]]=>Unit) {
        t.push(self, in) {
          case Success(res1, tail) => {
            f(Success(res1 :: Nil, tail))
            
            if ((!tail.isEmpty || first.size == 0) && (tail.isEmpty || (first.contains(in.head) || first.size == 0))) {      // lookahead}
              t.push(this, tail) {
                case Success(res2, tail) => f(Success(res1 :: res2, tail))
                case res: Failure => f(res)
              }
            }
          }
          
          case res: Failure => f(res)
        }
      }
      
      override def toString = self.toString + "+"
    }
    
    def ?(): Parser[Option[R]] = new NonTerminalParser[Option[R]] {
      def computeFirst(seen: Set[Parser[Any]]) = None
      
      def queue(t: Trampoline, in: Stream[Char])(f: Result[Option[R]]=>Unit) {
        f(Success(None, in))
        
        t.push(self, in) {
          case Success(res, tail) => f(Success(Some(res), tail))
          case res: Failure => f(res)
        }
      }
      
      override def toString = self.toString + "?"
    }
    
    def +? = (this+)?
    
    def ^^^[R2](v: =>R2) = map { _ => v }
  }
  
  trait TerminalParser[+R] extends Parser[R] { self =>
    final val terminal = true
    
    final def apply(in: Stream[Char]) = List(parse(in) match {
      case Success(res, tail) => processTail(tail) match {
        case Some(tail) => Success(res, tail)
        case None => Failure(TAIL_ERROR_PATTERN.format(tail.mkString), tail)
      }
      
      case x => x
    })
    
    /**
     * For terminal parsing, this just delegates back to apply()
     */
    def queue(t: Trampoline, in: Stream[Char])(f: Result[R]=>Unit) {
      f(parse(in))
    }
    
    protected def parse(in: Stream[Char]): Result[R]
    
    override def ~[R2](other: Parser[R2]) = other match {
      case other: TerminalParser[R2] => {
        new TerminalParser[~[R, R2]] {
          def computeFirst(s: Set[Parser[Any]]) = {
            val sub = self.computeFirst(s)
            sub flatMap { set =>
              if (set.size == 0)
                other.computeFirst(s)
              else
                sub
            }
          }
          
          def parse(in: Stream[Char]) = self.parse(in) match {
            case Success(res1, tail) => other.parse(tail) match {
              case Success(res2, tail) => Success(new ~(res1, res2), tail)
              case f: Failure => f
            }
            
            case f: Failure => f
          }
        }
      }
      
      case other => super.~(other)
    }
    
    def map[R2](f: R=>R2): Parser[R2] = new MappedParser[R, R2](self, f) with TerminalParser[R2] {
      def parse(in: Stream[Char]) = self.parse(in) match {
        case Success(res, tail) => Success(f(res), tail)
        case x: Failure => x
      }
    }
  }
  
  trait NonTerminalParser[+R] extends Parser[R] { self =>
    final val terminal = false
    
    /**
     * This method takes care of kicking off a <i>new</i>
     * parse process.  We will never call this method to
     * handle a sub-parse.  In such situations, we will use
     * the trampoline to queue results.
     * 
     * Note: to ensure greedy matching (for PEG compatibility)
     * we define any Success with a non-empty tail to be a
     * Failure
     */
    final def apply(in: Stream[Char]) = {
      val t = new Trampoline
      
      var successes = Set[Success[R]]()
      var failures = Set[Failure]()
      
      queue(t, in) {
        case s @ Success(_, Stream()) => successes += s
        
        case Success(res, tail) => {
          processTail(tail) match {
            case Some(tail) => successes += Success(res, tail)
            case None => failures += Failure(TAIL_ERROR_PATTERN.format(tail.mkString), tail)
          }
        }
        
        case f: Failure => failures += f
      }
      t.run()
      
      if (successes.isEmpty)
        failures.toList
      else
        successes.toList
    }
    
    def map[R2](f1: R=>R2): Parser[R2] = new MappedParser[R, R2](self, f1) with NonTerminalParser[R2] {
      def queue(t: Trampoline, in: Stream[Char])(f2: Result[R2]=>Unit) {
        self.queue(t, in) { 
          case Success(res, tail) => f2(Success(f1(res), tail))
          case f: Failure => f2(f)
        }
      }
    }
  }
  
  abstract class MappedParser[A, +B](private val p: Parser[A], private val f1: A=>B) extends Parser[B] {
    def computeFirst(s: Set[Parser[Any]]) = p.computeFirst(s + this)
  
    override def toString = p.toString
  
    override def equals(other: Any) = other match {
      case that: MappedParser[A, B] => {
        this.p == that.p && this.f1.getClass == that.f1.getClass
      }
  
      case _ => false
    }
  
    override def hashCode = p.hashCode + f1.getClass.hashCode
  }
  
  /**
   * Used for setting up a trampoline wrapper for disjunction
   * alternatives (for the sake of left-recursion).
   */
  private[gll] abstract class ThunkParser[+A](private val self: Parser[A]) extends NonTerminalParser[A] {
    def computeFirst(s: Set[Parser[Any]]) = self.computeFirst(s)
  
    override def toString = self.toString
  
    override def equals(other: Any) = other match {
      case that: ThunkParser[A] => this.self == that.self
      case _ => false
    }
  
    override def hashCode = self.hashCode
  }
  
  //////////////////////////////////////////////////////////////////////////////
  
  case class LiteralParser(str: String) extends TerminalParser[String] {
    def computeFirst(s: Set[Parser[Any]]) = {
      Some(if (str.length > 0) Set(str charAt 0) else Set())
    }
    
    def parse(in: Stream[Char]) = {
      val trunc = in take str.length
      lazy val errorMessage = "Expected '%s' got '%s'".format(str, trunc.mkString)
      
      if (trunc.lengthCompare(str.length) != 0) {
        Failure("Unexpected end of stream (expected '%s')".format(str), in)
      } else {
        val succ = trunc.zipWithIndex forall {
          case (c, i) => c == str.charAt(i)
        }
        
        if (succ)
          Success(str, in drop str.length)
        else
          Failure(errorMessage, in)
      }
    }
    
    override def equals(other: Any) = other match {
      case that: LiteralParser => this.str == that.str
      case _ => false
    }
    
    override def hashCode = str.hashCode
    
    override def toString = "'%s'".format(str)
  }
  
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
              if !in.isEmpty || p.first.size == 0
              if in.isEmpty || p.first.contains(in.head) || p.first.size == 0      // lookahead
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
  
  //////////////////////////////////////////////////////////////////////////////
  
  class Trampoline {
    private type Potential = (Parser[Any], Stream[Char])
    
    private val queue = new mutable.Queue[Potential]
    private val backlinks = mutable.Map[Potential, mutable.ListBuffer[Result[Any]=>Unit]]()
    private val popped = mutable.Map[Potential, mutable.ListBuffer[Result[Any]]]()
    
    def run() {
      while (queue.length > 0) {
        val (p, s, f) = pop()
        
        p.queue(this, s) { res =>
          popped((p, s)) += res             // store the result for late-comers
          f(res)
          
          tracef("Saved: %s *=> %s%n", (p, s), res)
        }
      }
    }
  
    def push[A](p: Parser[A], s: Stream[Char])(f: Result[A]=>Unit) {
      val tuple = (p, s)
      
      if (popped.contains(tuple) && popped(tuple).size > 0) {
        for (res <- popped(tuple)) {           // if we've already done that, use the result
          tracef("Revisited: %s *=> %s%n", tuple, res)
          f(res.asInstanceOf[Result[A]])
        }
      } else {
        if (!backlinks.contains(tuple)) {
          queue += tuple
          backlinks += (tuple -> new ListBuffer[Result[Any]=>Unit])
        }
        backlinks(tuple) += f.asInstanceOf[Result[Any]=>Unit]
  
        trace("Pushed: " + tuple)
      }
    }
    
    private def pop() = {
      val tuple @ (p, s) = queue.dequeue()
      val f = {
        val links = backlinks(tuple)
        
        if (links.size == 1)
          links.toList.head
        else
          { res: Result[Any] => links foreach { _(res) } }
      }
      
      backlinks -= tuple
      popped += (tuple -> new ListBuffer[Result[Any]])
  
      trace("Popped: " + tuple)
      
      (p, s, f)
    }
  }
}

// trivial companion object
object Parsers extends Parsers
