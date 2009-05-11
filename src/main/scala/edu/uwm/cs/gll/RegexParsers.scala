package edu.uwm.cs.gll

import scala.util.matching.Regex

import util._

// TODO need to handle trailing whitespace (somehow)
trait RegexParsers extends Parsers {
  protected val whitespace = """\s+"""r
  protected val skipWhitespace = true
  
  override implicit def literal(str: String): Parser[String] = {
    if (skipWhitespace) {
      new LiteralParser(str) {
        override def computeFirst(seen: Set[Parser[Any]]) = Some(UniversalOptCharSet)    // because of whitespace
        
        // there should be a way to do this with traits, but I haven't found it yet
        override def parse(s: LineStream) = super.parse(handleWhitespace(s))
      }
    } else super.literal(str)
  }
  
  implicit def regex(r: Regex): Parser[String] = {
    if (skipWhitespace) {
      new RegexParser(r) {
        override def computeFirst(seen: Set[Parser[Any]]) = Some(UniversalOptCharSet)
        
        override def parse(s: LineStream) = super.parse(handleWhitespace(s))
      }
    } else new RegexParser(r)
  }
  
  implicit def disjunctiveRegex(left: Regex) = new RichParser(regex(left))
  
  implicit def funRegexSyntax(p: Regex) = new RichSyntax1(regex(p))
  
  override protected def processTail(tail: LineStream) = {
    val newTail = if (skipWhitespace)
      handleWhitespace(tail)
    else
      tail
    
    super.processTail(newTail)
  }
  
  private def handleWhitespace(s: LineStream) =
    s.drop(whitespace findPrefixOf s map { _.length } getOrElse 0)
  
  case class RegexParser(private val regex: Regex) extends TerminalParser[String] {
    def computeFirst(s: Set[Parser[Any]]) = Some(UniversalOptCharSet)
    
    def parse(in: LineStream) = {
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
