package edu.uwm.cs.gll

import scala.util.matching.Regex

import util._

trait RegexParsers extends Parsers {
  import SetSyntax._
  
  protected val whitespace = """\s+"""r
  protected val skipWhitespace = true
  
  override implicit def literal(str: String): LiteralParser = {
    if (skipWhitespace) {
      new LiteralParser(str) {
        override def computeFirst(seen: Set[Parser[Any]]) = {
          val wsFirst = if (skipWhitespace) RegexUtils.first(whitespace) else Set[Option[Char]]()
          Some((wsFirst - None) ++ (super.computeFirst(seen) getOrElse Set[Option[Char]]()))
        }
        
        // there should be a way to do this with traits, but I haven't found it yet
        override def parse(s: LineStream) = super.parse(handleWhitespace(s))
      }
    } else super.literal(str)
  }
  
  implicit def regex(r: Regex): RegexParser = {
    if (skipWhitespace) {
      new RegexParser(r) {
        override def parse(s: LineStream) = super.parse(handleWhitespace(s))
      }
    } else new RegexParser(r)
  }
  
  override implicit def disjunctiveLiterals(left: String) = new RichParser(literal(left)) {
    def |(right: Regex) = new RegexParser(new Regex("(" + escapeRegex(left) + ")|(" + right + ")"))
    
    def |(right: String) = new RegexParser(new Regex("(" + escapeRegex(left) + ")|(" + escapeRegex(right) + ")"))
  }
  
  implicit def disjunctiveRegex(left: Regex) = new RichParser(regex(left)) {
    def |(right: Regex) = new RegexParser(new Regex("(" + left + ")|(" + right + ")"))
    
    def |(right: String) = new RegexParser(new Regex("(" + left + ")|(" + escapeRegex(right) + ")"))
  }
  
  implicit def funRegexSyntax(p: Regex) = new RichSyntax1(regex(p))
  
  override protected def processTail(tail: LineStream) = {
    val newTail = if (skipWhitespace)
      handleWhitespace(tail)
    else
      tail
    
    super.processTail(newTail)
  }
  
  private def escapeRegex(str: String) = {
    val specialChars = Set('[', ']', '{', '}', '\\', '|', '*', '+', '?', '^', '$', '(', ')')
    
    str.toCharArray.foldLeft("") { (str, c) =>
      if (specialChars contains c)
        str + '\\' + c
      else
        str + c
    }
  }
  
  private def handleWhitespace(s: LineStream) =
    s.drop(whitespace findPrefixOf s map { _.length } getOrElse 0)
  
  case class RegexParser(private val regex: Regex) extends TerminalParser[String] {
    if (regex == null)
      throw new NullPointerException("Cannot parse a null regular expression")
    
    lazy val constFirst = {
      val wsFirst = if (skipWhitespace) RegexUtils.first(whitespace) else Set[Option[Char]]()
      val regexFirst = RegexUtils.first(regex)
      
      if (wsFirst.isComplement)
        Some((wsFirst - None) ++ regexFirst)
      else
        Some(regexFirst ++ (wsFirst - None))
    }
    
    def computeFirst(s: Set[Parser[Any]]) = constFirst
    
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
