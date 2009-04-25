package edu.uwm.cs.gll

import scala.util.matching.Regex

// TODO need to handle trailing whitespace (somehow)
trait RegexParsers extends Parsers with CharSequenceConversions {
  protected val whitespace = """\s+"""r
  
  override implicit def literal(str: String): Parser[String] = new LiteralParser(str) {
    // there should be a way to do this with traits, but I haven't found it yet
    override def apply(s: Stream[Char]) = super.apply(handleWhitespace(s))
  }
  
  implicit def regex(r: Regex): Parser[String] = new RegexParser(r) {
    override def apply(s: Stream[Char]) = super.apply(handleWhitespace(s))
  }
  
  implicit def disjunctiveRegex(left: Regex) = new RichParser(regex(left))
  
  implicit def funRegexSyntax(p: Regex) = new RichSyntax1(regex(p))
  
  private def handleWhitespace(s: Stream[Char]) = {
    s.drop(whitespace findPrefixOf s map { _.length } getOrElse 0)
  }
}
