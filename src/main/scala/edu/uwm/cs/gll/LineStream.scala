package edu.uwm.cs.gll

import java.io.PrintStream

import scala.collection.LinearSeq
import scala.io.Source

/**
 * A lazy character stream with line awareness.  This also provides
 * amortized constant-time {@link CharSequence} access.
 */
sealed abstract class LineStream(val line: String, val lineNum: Int) extends LinearSeq[Char] with CharSequence { outer =>
  override def tail: LineStream = error("yeah, this is annoying")
  
  def charAt(i: Int) = apply(i)
  
  def subSequence(start: Int, end: Int) = this take end drop start
  
  override def take(length: Int): LineStream = {
    if (length < 0)
      throw new IllegalArgumentException(length.toString)
    else if (length > 0 && !isEmpty)
      new LineCons(head, tail take length - 1, line, lineNum)
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
    val charIndex = if (isEmpty)
      line.length
    else
      line.length - (tail takeWhile { _ != '\n' } length) - 1
    
    val caret = (1 to charIndex).foldLeft("") { (acc, _) => acc + ' ' } + '^'
    ps.print(pattern.format(lineNum, line, caret))
  }
}

object LineStream {
  val empty: LineStream = LineNil
  
  def apply(chars: Char*): LineStream = apply(new String(chars.toArray))
  
  def apply(str: String): LineStream = {
    val sep = System getProperty "line.separator"
    val src = Source fromString str
    
    val add = if (str.length > 0 && str.substring(str.length - sep.length, str.length) == sep)
      Iterator("")
    else
      Iterator.empty
    
    apply(src.getLines(sep) ++ add)
  }
  
  def apply(src: Source): LineStream = apply(src.getLines())
  
  def apply(lines: Iterator[String]): LineStream = {
    def gen(num: Int): LineStream = {
      if (lines.hasNext) {
        val line = lines.next
        
        val init = if (lines.hasNext)
          new LineCons('\n', gen(num + 1), line, num)
        else
          LineNil
        
        line.foldRight(init) { new LineCons(_, _, line, num) }
      } else LineNil
    }
    
    gen(1)
  }
  
  def unapplySeq(str: LineStream): Option[Seq[Char]] = Some(str)
}

class LineCons(override val head: Char, _tail: =>LineStream, line: String, lineNum: Int) extends LineStream(line, lineNum) {
  override lazy val tail = _tail
  
  override lazy val length = 1 + tail.length
  
  override val isEmpty = false
  
  def apply(i: Int) = if (i == 0) head else tail(i - 1)
}

object LineNil extends LineStream("", 1) {
  override val length = 0
  
  override val isEmpty = true
  
  def apply(i: Int) = throw new StringIndexOutOfBoundsException("Cannot index into an empty LineStream")
  
  override def head = throw new RuntimeException("Cannot get the head of an empty LineStream")
  
  override def tail = throw new RuntimeException("Cannot get the tail of an empty LineStream")
}
