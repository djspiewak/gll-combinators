package edu.uwm.cs.gll

import java.io.PrintStream
import scala.io.Source

import LineCons._

sealed abstract class LineStream(lineAndTail: (String, Option[LineStream]), val lineNum: Int) extends Seq[Char] { outer =>
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
    else {
      if (i == 0)
        head
      else
        tail(i - 1)
    }
  }
  
  override def lengthCompare(i: Int) = {
    if (isEmpty)
      i
    else if (i < 0)
      i
    else
      tail lengthCompare i - 1
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
  def printError(pattern: String)(ps: PrintStream) {
    val charIndex = line.length - (tail takeWhile { _ != '\n' } length)
    val caret = (1 to charIndex).foldLeft("") { (acc, _) => acc + ' ' } + '^'
    ps.print(pattern.format(lineNum, line, caret))
  }
}

object LineStream {
  val empty: LineStream = LineNil
  
  def apply(chars: Char*): LineStream = chars.foldRight(empty) { (c, tail) =>
    new LineCons(c, tail, tail.line, tail.lineNum)
  }
  
  def apply(str: String): LineStream = apply(str:_*)
  
  def apply(src: Source): LineStream = {
    def gen(lines: Iterator[String], num: Int): LineStream = {
      if (lines.hasNext) {
        val line = lines.next
        
        line.foldRight(new LineCons('\n', gen(lines, num + 1), line, num)) { new LineCons(_, _, line, num) }
      } else LineNil
    }
    
    gen(src.getLines, 1)
  }
  
  def unapply(str: LineStream) = Some(str)
}

class LineCons(val head: Char, _tail: =>LineStream, line: String, lineNum: Int) extends LineStream(constructLine(line, head, _tail), constructLineNum(lineNum, head)) {
  lazy val length = 1 + _tail.length
  override val isEmpty = false
  
  protected def constructTail = _tail
}

private object LineCons {
  
  def constructLine(line: String, head: Char, _tail: =>LineStream) = head match {
    case '\n' => {
      val tail = _tail
      val trunc = tail takeWhile { _ != '\n' }
      
      (trunc.mkString, Some(tail))
    }
    
    case _ => (line, None)
  }
  
  def constructLineNum(lineNum: Int, head: Char) = head match {
    case '\n' => lineNum + 1
    case _ => lineNum
  }
}

object LineNil extends LineStream(("", None), 1) {
  val length = 0
  override val isEmpty = true
  
  protected def constructTail = throw new AssertionError
  
  def head = throw new RuntimeException("Cannot get the head of an empty LineStream")
  
  override def tail = throw new RuntimeException("Cannot get the tail of an empty LineStream")
}
