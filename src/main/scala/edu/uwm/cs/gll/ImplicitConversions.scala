package edu.uwm.cs.gll

trait ImplicitConversions {
  implicit def literal(str: String): Parser[String] = new LiteralParser(str)
  
  implicit def disjunctiveSyntax[A](left: =>Parser[A]) = new RichParser(left)

  implicit def disjunctiveLiterals(left: String) = new RichParser(literal(left))

  class RichParser[A](left: =>Parser[A]) {
    def |[B >: A](right: =>Parser[B]): Parser[B] = new DisjunctiveParser(left, right)
  }
}
