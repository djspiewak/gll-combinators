package edu.uwm.cs.gll

import java.io.{PrintStream, StringReader, Reader}

import scala.collection.LinearSeq
import scala.io.Source

/**
 * A lazy character stream with line awareness.  This also provides
 * amortized constant-time {@link CharSequence} access.
 */
sealed abstract class LineStream(val line: String, val lineNum: Int, val colNum: Int) extends LinearSeq[Char] with CharSequence { outer =>
  override def tail: LineStream = error("yeah, this is annoying")
  
  def charAt(i: Int) = apply(i)
  
  def subSequence(start: Int, end: Int) = this take end drop start
  
  override def take(length: Int): LineStream = {
    if (length < 0)
      throw new IllegalArgumentException(length.toString)
    else if (length > 0 && !isEmpty)
      new LazyLineCons(head, tail take length - 1, line, lineNum, colNum)
    else
      LineNil
  }
  
  override def drop(length: Int): LineStream = {
    if (length < 0)
      throw new IllegalArgumentException(length.toString)
    else if (length > 0 && !isEmpty)
      tail drop length - 1
    else if (length == 0)
      this
    else
      throw new IndexOutOfBoundsException(length.toString)
  }
  
  override def lengthCompare(i: Int) = {
    if (isEmpty)
      -i
    else if (i <= 0)
      1
    else
      tail lengthCompare i - 1
  }
  
  def zip(that: LineStream): Stream[(Char, Char)] = {
    if (this.isEmpty || that.isEmpty)
      Stream.empty
    else
      Stream.cons((this.head, that.head), tail zip that)
  }
  
  def zipWithIndex = {
    def gen(tail: LineStream, i: Int): Stream[(Char, Int)] = {
      if (tail.isEmpty)
        Stream.empty
      else
        Stream.cons((tail.head, i), gen(tail.tail, i + 1))
    }
    
    gen(this, 0)
  }
  
  override def toString = mkString
  
  /**
   * Expects a pattern with the following arguments:
   * 
   * <ol>
   *   <li><code>%d</code> &mdash; Line number</li>
   *   <li><code>%s</code> &mdash; Line contents</li>
   *   <li><code>%s</code> &mdash; Indicator caret</li>
   * </ol>
   */
  def printError(pattern: String)(ps: PrintStream) {
    ps.print(formatError(pattern))
  }
  
  /**
   * Expects a pattern with the following arguments:
   * 
   * <ol>
   *   <li><code>%d</code> &mdash; Line number</li>
   *   <li><code>%s</code> &mdash; Line contents</li>
   *   <li><code>%s</code> &mdash; Indicator caret</li>
   * </ol>
   */
  def formatError(pattern: String): String = {
    val caret = (1 to colNum).foldLeft("") { (acc, _) => acc + ' ' } + '^'
    pattern.format(lineNum, line, caret)
  }
  
  override def equals(a: Any) = a match {
    case that: LineStream => this eq that
    case _ => false
  }
  
  override def hashCode = lineNum * colNum
}

object LineStream {
  val empty: LineStream = LineNil
  
  def apply(chars: Char*): LineStream = apply(new String(chars.toArray))
  
  def apply(str: String): LineStream = apply(new StringReader(str))
  
  def apply(itr: Iterator[Char]): LineStream = {
    apply(new Reader {
      def close() {}
      
      def read(cbuf: Array[Char], off: Int, len: Int) = {
        val emptyAtStart = itr.isEmpty
        val proj = itr take len
        
        var count = 0
        while (proj.hasNext) {
          cbuf(count) = proj.next()
          count += 1
        }
        
        if (emptyAtStart) -1 else count
      }
    })
  }
  
  def apply(reader: Reader): LineStream = {
    def gen(num: Int): LineStream = {
      def state0(acc: StringBuilder): (String, String) = {
        val c = reader.read()
        
        if (c < 0) 
          (acc.toString, "")
        else if (c == '\n') 
          (acc.toString, "\n")
        else if (c == '\r') 
          state1(acc)
        else 
          state0(acc.append(c.toChar))
      }
      
      def state1(acc: StringBuilder): (String, String) = {
        val c = reader.read()
        
        if (c < 0) 
          (acc.append('\r').toString, "")
        else if (c == '\n') 
          (acc.toString, "\r\n")
        else if (c == '\r') 
          state1(acc.append('\r'))
        else 
          state0(acc.append('\r').append(c.toChar))
      }
      
      val (line, term) = state0(new StringBuilder)
      
      // cannot use a fold because we don't have a non-strict foldRight
      val termLS = if (term.length == 0)
        LineNil
      else if (term.length == 1)
        new LazyLineCons(term.first, gen(num + 1), line, num, line.length + 1)
      else if (term.length == 2)
        new StrictLineCons(term(0), new LazyLineCons(term(1), gen(num + 1), line, num, line.length + 2), line, num, line.length + 1)
      else
        error("Line terminator contains more than two characters; cannot process newline!")
      
      val (back, _) = line.foldRight((termLS, line.length)) {
        case (c, (tail, colNum)) => (new StrictLineCons(c, tail, line, num, colNum), colNum - 1)
      }
      
      back
    }
    
    gen(1)
  }
  
  def unapplySeq(str: LineStream): Option[Seq[Char]] = Some(str)
}

class LazyLineCons(override val head: Char, _tail: =>LineStream, line: String, lineNum: Int, colNum: Int) extends LineStream(line, lineNum, colNum) {
  override lazy val tail = _tail
  
  override lazy val length = 1 + tail.length
  
  override val isEmpty = false
  
  def apply(i: Int) = if (i == 0) head else tail(i - 1)
}

class StrictLineCons(override val head: Char, override val tail: LineStream, line: String, lineNum: Int, colNum: Int) extends LineStream(line, lineNum, colNum) {
  override lazy val length = 1 + tail.length
  
  override val isEmpty = false
  
  def apply(i: Int) = if (i == 0) head else tail(i - 1)
}

object LineNil extends LineStream("", 1, 1) {
  override val length = 0
  
  override val isEmpty = true
  
  def apply(i: Int) = throw new StringIndexOutOfBoundsException("Cannot index into an empty LineStream")
  
  override def head = throw new RuntimeException("Cannot get the head of an empty LineStream")
  
  override def tail = throw new RuntimeException("Cannot get the tail of an empty LineStream")
}
