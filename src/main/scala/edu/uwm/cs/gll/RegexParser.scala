package edu.uwm.cs.gll

import scala.util.matching.Regex
import StreamUtils._

case class RegexParser(private val regex: Regex) extends TerminalParser[String] with CharSequenceConversions {
  def computeFirst(s: Set[Parser[Any]]) = Some(UniversalCharSet)
  
  def apply(in: Stream[Char]) = {
    val res = regex findPrefixOf in map { str => Success(str, in drop str.length) }
    (res getOrElse Failure("Expected /%s/".format(regex), in)) :: Nil
  }
  
  override def equals(other: Any) = other match {
    case that: RegexParser => this.regex.toString == that.regex.toString
    case _ => false
  }
  
  override def hashCode = regex.toString.hashCode
  
  override def toString = "/%s/".format(regex)
}
