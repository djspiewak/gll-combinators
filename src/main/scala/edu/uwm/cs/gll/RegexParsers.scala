package edu.uwm.cs.gll

import scala.util.matching.Regex

import util._

// TODO need to handle trailing whitespace (somehow)
trait RegexParsers extends Parsers with ImplicitConversions with CharSequenceConversions {
  protected val whitespace = """\s+"""r
  
  override implicit def literal(str: String): Parser[String] = new LiteralParser(str) {
    // there should be a way to do this with traits, but I haven't found it yet
    override def parse(s: Stream[Char]) = super.parse(handleWhitespace(s))
  }
  
  implicit def regex(r: Regex): Parser[String] = new RegexParser(r) {
    override def parse(s: Stream[Char]) = super.parse(handleWhitespace(s))
  }
  
  implicit def disjunctiveRegex(left: Regex) = new RichParser(regex(left))
  
  implicit def funRegexSyntax(p: Regex) = new RichSyntax1(regex(p))
  
  override protected def processTail(tail: Stream[Char]) = 
    super.processTail(handleWhitespace(tail))
  
  private def handleWhitespace(s: Stream[Char]) =
    s.drop(whitespace findPrefixOf s map { _.length } getOrElse 0)
  
  case class RegexParser(private val regex: Regex) extends TerminalParser[String] with CharSequenceConversions {
    def computeFirst(s: Set[Parser[Any]]) = Some(UniversalCharSet)
    
    def parse(in: Stream[Char]) = {
      val res = regex findPrefixOf in map { str => Success(str, in drop str.length) }
      res getOrElse Failure("Expected /%s/".format(regex), in)
    }
    
    override def equals(other: Any) = other match {
      case that: RegexParser => this.regex.toString == that.regex.toString
      case _ => false
    }
    
    override def hashCode = regex.toString.hashCode
    
    override def toString = "/%s/".format(regex)
  }
}

object RegexParsers extends RegexParsers
