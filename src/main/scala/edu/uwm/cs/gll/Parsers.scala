package edu.uwm.cs.gll

trait Parsers extends ImplicitConversions {
  implicit def literal(str: String): Parser[String] = new LiteralParser(str)
  
  def opt[A](p: Parser[A]) = p?
  
  def rep[A](p: Parser[A]) = p*
  
  def rep1[A](p: Parser[A]) = p+
}
