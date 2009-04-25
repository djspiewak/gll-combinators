package edu.uwm.cs.gll

trait Parsers extends ImplicitConversions {
  implicit def literal(str: String): Parser[String] = new LiteralParser(str)
}
