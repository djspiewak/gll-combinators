package edu.uwm.cs.gll

trait ImplicitConversions {
  implicit def literal(str: String): Parser[String] = new LiteralParser(str)
  
  implicit def disjunctiveSyntax[A](left: =>Parser[A])= new {
    def |(right: =>Parser[A]): Parser[A] = new DisjunctiveParser(left, right)
    
    def |(right: String) = new DisjunctiveParser(left, literal(right))
  }
  
  implicit def disjunctiveLiterals(left: String) = new {
    def |[A](right: =>Parser[A]) = new DisjunctiveParser(literal(left), right)
    
    def |(right: String): Parser[String] = new DisjunctiveParser(literal(left), literal(right))
    
    def ^^[A](f: String=>A): Parser[A] = literal(left) ^^ f
    
    def ^^^[A](f: =>A): Parser[A] = literal(left) ^^^ f
  }
}
