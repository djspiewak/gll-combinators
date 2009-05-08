package edu.uwm.cs.gll

import java.io.PrintStream
import scala.io.Source

/**
 * A lazy character stream with line awareness.  This also provides
 * amortized constant-time {@link CharSequence} access.
 */
sealed abstract class LineStream(val line: String, val lineNum: Int) extends Seq[Char] with CharSequence { outer =>
  private var _page: String = ""
  private val pageLock = new AnyRef
  
  def head: Char
  
  def tail: LineStream
  
  def elements = new Iterator[Char] {
    var cur = outer
    
    def hasNext = !cur.isEmpty
    
    def next = {
      val back = cur.head
      cur = cur.tail
      back
    }
  }
  
  def apply(i: Int) = {
    if (lengthCompare(i) < 0)
      throw new IndexOutOfBoundsException(i.toString)
    else if (i == 0)      // trivial case
      head
    else
      page(i + 1) charAt i
  }
  
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
    val charIndex = line.length - (tail takeWhile { _ != '\n' } length) - 1
    val caret = (1 to charIndex).foldLeft("") { (acc, _) => acc + ' ' } + '^'
    ps.print(pattern.format(lineNum, line, caret))
  }
  
  private def page(length: Int) = pageLock synchronized {
    if (length > _page.length) {
      var newLength = Math.max(_page.length, 10)
      while (length > newLength)
        newLength *= 2
      
      _page = this take newLength mkString
    }
    
    _page
  }
}

object LineStream {
  val empty: LineStream = LineNil
  
  def apply(chars: Char*): LineStream = apply(new String(chars.toArray))
  
  def apply(str: String): LineStream = apply(Source fromString str)
  
  def apply(src: Source): LineStream = apply(src.getLines)
  
  def apply(lines: Iterator[String]): LineStream = {
    def gen(num: Int): LineStream = {
      if (lines.hasNext) {
        val line = lines.next
        val trimmed = line.trim
        
        val init = new LineCons(line(line.length - 1), gen(num + 1), trimmed, num)
        line.substring(0, line.length - 1).foldRight(init) { new LineCons(_, _, trimmed, num) }
      } else LineNil
    }
    
    gen(1)
  }
  
  def unapplySeq(str: LineStream): Option[Seq[Char]] = Some(str)
}

class LineCons(val head: Char, _tail: =>LineStream, line: String, lineNum: Int) extends LineStream(line, lineNum) {
  lazy val tail = _tail
  
  lazy val length = 1 + tail.length
  
  override val isEmpty = false
}

object LineNil extends LineStream("", 1) {
  val length = 0
  override val isEmpty = true
  
  def head = throw new RuntimeException("Cannot get the head of an empty LineStream")
  
  def tail = throw new RuntimeException("Cannot get the tail of an empty LineStream")
}
