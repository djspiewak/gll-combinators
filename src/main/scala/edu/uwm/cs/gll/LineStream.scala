package edu.uwm.cs.gll

import java.io.PrintStream
import scala.io.Source

/**
 * A lazy character stream with line awareness.  This also provides
 * amortized constant-time {@link CharSequence} access.
 */
sealed abstract class LineStream(lineAndTail: (String, Option[LineStream]), val lineNum: Int) extends Seq[Char] with CharSequence { outer =>
  private var _page: String = ""
  private val pageLock = new AnyRef
  
  private var _tail = lineAndTail._2
  private val tailLock = new AnyRef
  
  val line = lineAndTail._1
  
  def head: Char
  
  def tail = tailLock synchronized {
    _tail match {
      case Some(tail) => tail
      
      case None => {
        _tail = Some(constructTail)
        _tail.get
      }
    }
  }
  
  protected def constructTail: LineStream
  
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
  
  def subSequence(start: Int, end: Int) = page(end).subSequence(start, end)
  
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

class LineCons(val head: Char, _tail: =>LineStream, line: String, lineNum: Int) extends LineStream((line, None), lineNum) {
  lazy val length = 1 + _tail.length
  
  override val isEmpty = false
  
  protected def constructTail = _tail
}

object LineNil extends LineStream(("", None), 1) {
  val length = 0
  override val isEmpty = true
  
  protected def constructTail = throw new AssertionError
  
  def head = throw new RuntimeException("Cannot get the head of an empty LineStream")
  
  override def tail = throw new RuntimeException("Cannot get the tail of an empty LineStream")
}
