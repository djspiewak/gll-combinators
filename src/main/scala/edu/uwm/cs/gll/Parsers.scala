package edu.uwm.cs.gll

import scala.collection.mutable
import mutable.{Buffer, ListBuffer}

import util._

import Global._

// I hate the way this file is organized, but I don't have a choice
trait Parsers {
  private val TAIL_ERROR_PATTERN = "Unexpected trailing characters: '%s'"
  
  implicit def literal(str: String): Parser[String] = new LiteralParser(str)
  
  def opt[A](p: Parser[A]) = p?
  
  def rep[A](p: Parser[A]) = p*
  
  def rep1[A](p: Parser[A]) = p+
  
  protected def processTail(tail: LineStream) = if (tail.isEmpty) Some(tail) else None
  
  private def canonicalize(str: String) = str.foldLeft("") { (back, c) =>
    val tack = c match {
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\f' => "\\f"
      case c => c.toString
    }
    
    back + tack
  }
  
  private implicit def setSyntax[A](set: Set[A]) = new {
    def isComplement = set.isInstanceOf[ComplementarySet[_]]
    
    def complement = new ComplementarySet(set)
  }
  
  // implicit conversions
  
  implicit def disjunctiveSyntax[A](left: =>Parser[A]) = new RichParser(left)
  implicit def disjunctiveLiterals(left: String) = new RichParser(literal(left))
  
  implicit def funSyntax1[A](p: Parser[A]) = new RichSyntax1(p)
  implicit def funLitSyntax(p: String) = new RichSyntax1(literal(p))
  implicit def funSyntax2[A, B](p: Parser[A ~ B]) = new RichSyntax2(p)
  
  implicit def funSyntax3l[A, B, C](p: Parser[A ~ B ~ C]) = new RichSyntax3l(p)
  implicit def funSyntax3r[A, B, C](p: Parser[~[A, B ~ C]]) = new RichSyntax3r(p)
  
  implicit def funSyntax4ll[A, B, C, D](p: Parser[A ~ B ~ C ~ D]) = new RichSyntax4ll(p)
  implicit def funSyntax4lr[A, B, C, D](p: Parser[~[A, B ~ C] ~ D]) = new RichSyntax4lr(p)
  implicit def funSyntax4rl[A, B, C, D](p: Parser[~[A, B ~ C ~ D]]) = new RichSyntax4rl(p)
  implicit def funSyntax4rr[A, B, C, D](p: Parser[~[A, ~[B, C ~ D]]]) = new RichSyntax4rr(p)
  
  implicit def funSyntax5lll[A, B, C, D, E](p: Parser[A ~ B ~ C ~ D ~ E]) = new RichSyntax5lll(p)
  implicit def funSyntax5llr[A, B, C, D, E](p: Parser[~[A, B ~ C] ~ D ~ E]) = new RichSyntax5llr(p)
  implicit def funSyntax5lrl[A, B, C, D, E](p: Parser[~[A, B ~ C ~ D] ~ E]) = new RichSyntax5lrl(p)
  implicit def funSyntax5lrr[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D]] ~ E]) = new RichSyntax5lrr(p)
  implicit def funSyntax5rll[A, B, C, D, E](p: Parser[~[A ~ B, C ~ D ~ E]]) = new RichSyntax5rll(p)
  implicit def funSyntax5rlr[A, B, C, D, E](p: Parser[~[A ~ B, ~[C, D ~ E]]]) = new RichSyntax5rlr(p)
  implicit def funSyntax5rrl[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D ~ E]]]) = new RichSyntax5rrl(p)
  implicit def funSyntax5rrr[A, B, C, D, E](p: Parser[~[A, ~[B, ~[C, D ~ E]]]]) = new RichSyntax5rrr(p)
  
  implicit def funSyntax6[A, B, C, D, E, F](p: Parser[A ~ B ~ C ~ D ~ E ~ F]) = new RichSyntax6(p)
  implicit def funSyntax7[A, B, C, D, E, F, G](p: Parser[A ~ B ~ C ~ D ~ E ~ F ~ G]) = new RichSyntax7(p)

  class RichParser[A](left: =>Parser[A]) {
    def |[B >: A](right: =>Parser[B]): Parser[B] = new DisjunctiveParser(left, right)
  }
  
  // map syntax
  
  class RichSyntax1[A](p: Parser[A]) {
    def ^^[R](f: A=>R) = p map f
  }
  
  class RichSyntax2[A, B](p: Parser[A ~ B]) {
    def ^^[R](fun: (A, B)=>R) = p map { case a ~ b => fun(a, b) }
  }
  
  class RichSyntax3l[A, B, C](p: Parser[A ~ B ~ C]) {
    def ^^[R](fun: (A, B, C)=>R) = p map { case a ~ b ~ c => fun(a, b, c) }
  }
  
  class RichSyntax3r[A, B, C](p: Parser[~[A, B ~ C]]) {
    def ^^[R](fun: (A, B, C)=>R) = p map { case a ~ (b ~ c) => fun(a, b, c) }
  }
  
  class RichSyntax4ll[A, B, C, D](p: Parser[A ~ B ~ C ~ D]) {
    def ^^[R](fun: (A, B, C, D)=>R) = p map { case a ~ b ~ c ~ d => fun(a, b, c, d) }
  }
  
  class RichSyntax4lr[A, B, C, D](p: Parser[~[A, B ~ C] ~ D]) {
    def ^^[R](fun: (A, B, C, D)=>R) = p map { case a ~ (b ~ c) ~ d => fun(a, b, c, d) }
  }
  
  class RichSyntax4rl[A, B, C, D](p: Parser[~[A, B ~ C ~ D]]) {
    def ^^[R](fun: (A, B, C, D)=>R) = p map { case a ~ ((b ~ c) ~ d) => fun(a, b, c, d) }
  }
  
  class RichSyntax4rr[A, B, C, D](p: Parser[~[A, ~[B, C ~ D]]]) {
    def ^^[R](fun: (A, B, C, D)=>R) = p map { case a ~ (b ~ (c ~ d)) => fun(a, b, c, d) }
  }
  
  class RichSyntax5lll[A, B, C, D, E](p: Parser[A ~ B ~ C ~ D ~ E]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case a ~ b ~ c ~ d ~ e => fun(a, b, c, d, e) }
  }
  
  class RichSyntax5llr[A, B, C, D, E](p: Parser[~[A, B ~ C] ~ D ~ E]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case (a ~ (b ~ c)) ~ d ~ e => fun(a, b, c, d, e) }
  }
  
  class RichSyntax5lrl[A, B, C, D, E](p: Parser[~[A, B ~ C ~ D] ~ E]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case (a ~ (b ~ c ~ d)) ~ e => fun(a, b, c, d, e) }
  }
  
  class RichSyntax5lrr[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D]] ~ E]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case (a ~ (b ~ (c ~ d))) ~ e => fun(a, b, c, d, e) }
  }
  
  class RichSyntax5rll[A, B, C, D, E](p: Parser[~[A ~ B, C ~ D ~ E]]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case (a ~ b) ~ (c ~ d ~ e) => fun(a, b, c, d, e) }
  }
  
  class RichSyntax5rlr[A, B, C, D, E](p: Parser[~[A ~ B, ~[C, D ~ E]]]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case (a ~ b) ~ (c ~ (d ~ e)) => fun(a, b, c, d, e) }
  }
  
  class RichSyntax5rrl[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D ~ E]]]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case a ~ (b ~ (c ~ d ~ e)) => fun(a, b, c, d, e) }
  }
  
  class RichSyntax5rrr[A, B, C, D, E](p: Parser[~[A, ~[B, ~[C, D ~ E]]]]) {
    def ^^[R](fun: (A, B, C, D, E)=>R) = p map { case a ~ (b ~ (c ~ (d ~ e))) => fun(a, b, c, d, e) }
  }
  
  class RichSyntax6[A, B, C, D, E, F](p: Parser[A ~ B ~ C ~ D ~ E ~ F]) {
    def ^^[R](fun: (A, B, C, D, E, F)=>R) = p map { case a ~ b ~ c ~ d ~ e ~ f => fun(a, b, c, d, e, f) }
  }
  
  class RichSyntax7[A, B, C, D, E, F, G](p: Parser[A ~ B ~ C ~ D ~ E ~ F ~ G]) {
    def ^^[R](fun: (A, B, C, D, E, F, G)=>R) = p map { case a ~ b ~ c ~ d ~ e ~ f ~ g => fun(a, b, c, d, e, f, g) }
  }
  
  //////////////////////////////////////////////////////////////////////////////
  
  sealed trait Parser[+R] extends (LineStream=>List[Result[R]]) { self =>
    val terminal: Boolean
    
    lazy val first = {
      val set = computeFirst(Set()) getOrElse Set()
      
      if (set contains None)
        UniversalCharSet         // if \epsilon \in FIRST
      else
        set flatMap { x => x }
    }
    
    /**
     * @return The FIRST set for this parser, or the empty set
     *         if the production goes to \epsilon.
     */
    def computeFirst(seen: Set[Parser[Any]]): Option[Set[Option[Char]]]
    
    def queue(t: Trampoline, in: LineStream)(f: Result[R]=>Unit)
    
    // syntax
    
    def apply(str: String): List[Result[R]] = apply(LineStream(str))
    
    def map[R2](f: R=>R2): Parser[R2]
    
    def flatMap[R2](f1: R=>Parser[R2]): Parser[R2] = new NonTerminalParser[R2] {
      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen + this)
      
      def queue(t: Trampoline, in: LineStream)(f2: Result[R2]=>Unit) {
        self.queue(t, in) {
          case Success(res1, tail) => f1(res1).queue(t, tail)(f2)
          case f: Failure => f2(f)
        }
      }
    }
    
    def filter(f: R=>Boolean): Parser[R] = new NonTerminalParser[R] {
      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen + this)
      
      def queue(t: Trampoline, in: LineStream)(f2: Result[R]=>Unit) {
        self.queue(t, in) {
          case s @ Success(res, _) => {
            if (f(res))
              f2(s)
            else
              f2(Failure("Syntax error", in))
          }
          
          case f: Failure => f2(f)
        }
      }
    }
    
    def orElse[R2 >: R](alternate: =>Parser[R2]): Parser[R2] = new DisjunctiveParser(this, alternate)
    
    // operators
    
    def ~[R2](that: Parser[R2]): Parser[R ~ R2] = new SequentialParser(this, that)
    
    def <~[R2](that: Parser[R2]) = this ~ that map { case a ~ _ => a }
    
    def ~>[R2](that: Parser[R2]) = this ~ that map { case _ ~ b => b }
    
    def *(): Parser[List[R]] = (this+?) map { _ getOrElse Nil }
    
    def *(sep: Parser[_]): Parser[List[R]] = (this + sep).? ^^ { _ getOrElse Nil }
    
    def +(): Parser[List[R]] = new NonTerminalParser[List[R]] {
      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen + this)
      
      def queue(t: Trampoline, in: LineStream)(f: Result[List[R]]=>Unit) {
        t.add(self, in) {
          case Success(res1, tail) => {
            f(Success(res1 :: Nil, tail))
            
            if ((!tail.isEmpty || first.size == 0) && (tail.isEmpty || (first.contains(in.head) || first.size == 0))) {      // lookahead}
              t.add(this, tail) {
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
    
    def +(sep: Parser[_]) = this ~ (sep ~> this).* ^^ { _ :: _ }
    
    def ?(): Parser[Option[R]] = new NonTerminalParser[Option[R]] {
      def computeFirst(seen: Set[Parser[Any]]) = None
      
      def queue(t: Trampoline, in: LineStream)(f: Result[Option[R]]=>Unit) {
        f(Success(None, in))
        
        t.add(self, in) {
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
    
    final def apply(in: LineStream) = List(parse(in) match {
      case Success(res, tail) => processTail(tail) match {
        case Some(tail) => Success(res, tail)
        case None => Failure(TAIL_ERROR_PATTERN.format(canonicalize(tail.mkString)), tail)
      }
      
      case x => x
    })
    
    /**
     * For terminal parsing, this just delegates back to apply()
     */
    def queue(t: Trampoline, in: LineStream)(f: Result[R]=>Unit) {
      f(parse(in))
    }
    
    protected def parse(in: LineStream): Result[R]
    
    override def ~[R2](other: Parser[R2]) = other match {
      case other: TerminalParser[R2] => {
        new TerminalParser[R ~ R2] {
          def computeFirst(s: Set[Parser[Any]]) = {
            val sub = self.computeFirst(s)
        
            sub map { set =>
              if (set.size == 0 || set.contains(None))
                other.computeFirst(s) match {
                  case Some(set2) => {
                    if (set.isComplement)
                      (set - None) ++ set2
                    else
                      set2 ++ (set - None)
                  }
                  
                  case None => set
                }
              else
                set
            }
          }
          
          def parse(in: LineStream) = self.parse(in) match {
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
      def parse(in: LineStream) = self.parse(in) match {
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
    final def apply(in: LineStream) = {
      val t = new Trampoline
      
      var successes = Set[Success[R]]()
      var failures = Set[Failure]()
      
      queue(t, in) {
        case s @ Success(_, Stream()) => successes += s
        
        case Success(res, tail) => {
          processTail(tail) match {
            case Some(tail) => successes += Success(res, tail)
            case None => failures += Failure(TAIL_ERROR_PATTERN.format(canonicalize(tail.mkString)), tail)
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
      def queue(t: Trampoline, in: LineStream)(f2: Result[R2]=>Unit) {
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
      Some(if (str.length > 0) Set(Some(str charAt 0)) else Set(None))
    }
    
    def parse(in: LineStream) = {
      val trunc = in take str.length
      lazy val errorMessage = "Expected '%s' got '%s'".format(str, canonicalize(trunc.mkString))
      
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
  
  class SequentialParser[+A, +B](private val left: Parser[A], private val right: Parser[B]) extends NonTerminalParser[A ~ B] {
    def computeFirst(seen: Set[Parser[Any]]) = {
      if (seen contains this) None    // left-recursion detected!
      else {
        val newSeen = seen + this
        val sub = left.computeFirst(newSeen)
        
        sub map { set =>
          if (set.size == 0 || set.contains(None))
            right.computeFirst(newSeen) match {
              case Some(set2) => {
                if (set.isComplement)
                  (set - None) ++ set2
                else
                  set2 ++ (set - None)
              }
              
              case None => set
            }
          else
            set
        }
      }
    }
    
    def queue(t: Trampoline, in: LineStream)(f: Result[A ~ B]=>Unit) {
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
      gather.foldLeft(Map[Char, Parser[A]]()) { (map, p) =>
        p.first.foldLeft(map) { _(_) = p }
      }
    }
  
    /**
     * Checks if all FIRST sets are disjoint and none
     * are empty.  This is convergent even for
     * left-recursive parsers.
     */
    lazy val isLL1 = {
      val sets = gather map { _.first }
      val areFinite = sets forall { !_.isComplement }
      
      if (areFinite) {
        val totalSize = sets.foldLeft(0) { _ + _.size }
        val union = sets.reduceLeft[Set[Char]] { _ ++ _ }
        (totalSize == union.size) && (sets forall { _.size > 0 })
      } else false
    }
    
    def computeFirst(seen: Set[Parser[Any]]) = {
      if (seen contains this)
        None          // left-recursion detected!
      else {
        val newSeen = seen + this
        
        val leftFirst = left.computeFirst(newSeen) getOrElse Set()
        val rightFirst = right.computeFirst(newSeen) getOrElse Set()
        
        val back = if (rightFirst.isComplement)
          rightFirst ++ leftFirst
        else
          leftFirst ++ rightFirst
        
        Some(if (back.size == 0)
          UniversalOptCharSet
        else
          back)
      }
    }
    
    def queue(t: Trampoline, in: LineStream)(f: Result[A]=>Unit) {
      val UNEXPECTED_PATTERN = "Unexpected value in stream: '"
      
      if (isLL1) {        // graceful degrade to LL(1)
        trace("Detected LL(1): " + this)
        
        if (in.isEmpty) {
          f(Failure("Unexpected end of stream", in))
        } else {
          predict get in.head match {
            case Some(p) => p.queue(t, in)(f)
            
            case None => f(Failure(UNEXPECTED_PATTERN + in.head + "'", in))
          }
        }
      } else {
        val thunk = new ThunkParser(this) {
          def queue(t: Trampoline, in: LineStream)(f: Result[A]=>Unit) {
            var predicted = false
            val results = mutable.Set[Result[A]]()    // merge results
            
            for {
              p <- gather
              
              // [(S = {}) -> (FIRST = U)] /\ [~(S = {}) -> (S[0] \in FIRST)]
              if !in.isEmpty || p.first == UniversalCharSet
              if in.isEmpty || p.first.contains(in.head)      // lookahead
            } {
              predicted = true
              t.add(p, in) { res =>
                if (!results.contains(res)) {
                  tracef("Reduced: %s *=> %s%n", this, res)
    
                  f(res)
                  results += res
                }
              }
            }
            
            if (!predicted) {
              if (in.isEmpty)
                f(Failure("Unexpected end of stream", in))
              else
                f(Failure(UNEXPECTED_PATTERN + in.head + "'", in))
            }
          }
        }
  
        t.add(thunk, in)(f)
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
    
    override def toString = {
      val tail = """.*\$([^\$]+\$\d+)$"""r
      
      val tail(left) = leftClass.getName
      val tail(right) = rightClass.getName
      
      "(%s | %s)".format(left, right)
    }
  }
  
  //////////////////////////////////////////////////////////////////////////////
  
  class Trampoline {
    private type RSet[A] = mutable.Set[Result[A]]
    private type SSet[A] = mutable.Set[Success[A]]
    private type FSet[A] = mutable.Set[Result[A]=>Unit]
    
    // R
    private val queue = new mutable.Stack[(Parser[Any], LineStream)]
    
    // U_j
    private val done = mutable.Map[LineStream, mutable.Set[Parser[Any]]]()
    
    // P
    private val popped = mutable.Map[LineStream, HOMap[Parser, SSet]]()
    
    // GSS back edges
    private val backlinks = mutable.Map[LineStream, HOMap[Parser, FSet]]()
    
    // prevents divergence in cyclic GSS traversal
    private val saved = HOMap[Result, FSet]()
    
    // L_0
    def run() {
      while (!queue.isEmpty) {
        val (p, s) = remove()
        
        p.queue(this, s) { res =>
          if (!popped.contains(s))
            popped += (s -> HOMap[Parser, SSet]())
        
          if (!popped(s).contains(p))
            popped(s) += (p -> new mutable.HashSet[Success[Any]])
        
          res match {
            case succ: Success[Any] => {
              popped(s)(p) += succ
              tracef("Saved: %s *=> %s%n", (p, s), succ)
            }
            
            case _: Failure => ()
          }
        
          if (!saved.contains(res))
            saved += (res -> new mutable.HashSet[Result[Any]=>Unit])
          
          for (f <- backlinks(s)(p)) {
            if (!saved(res).contains(f)) {
              saved(res) += f
              f(res)
            }
          }
        }
      }
    }
  
    def add[A](p: Parser[A], s: LineStream)(f: Result[A]=>Unit) {
      val tuple = (p, s)
      
      if (!backlinks.contains(s))
        backlinks += (s -> HOMap[Parser, FSet]())
      
      if (!backlinks(s).contains(p))
        backlinks(s) += (p -> new mutable.HashSet[Result[Any]=>Unit])
      
      backlinks(s)(p) += f
      
      if (popped.contains(s) && popped(s).contains(p)) {
        for (res <- popped(s)(p)) {           // if we've already done that, use the result
          tracef("Revisited: %s *=> %s%n", tuple, res)
          f(res)
        }
      } else {
        if (!done.contains(s))
          done += (s -> new mutable.HashSet[Parser[Any]])
        
        if (!done(s).contains(p)) {
          queue += tuple
          done(s) += p
          
          trace("Added: " + tuple)
        }
      }
    }
    
    private def remove() = {
      val tuple = queue.pop()
      trace("Removed: " + tuple)
      
      tuple
    }
  }
}

// trivial companion object
object Parsers extends Parsers
