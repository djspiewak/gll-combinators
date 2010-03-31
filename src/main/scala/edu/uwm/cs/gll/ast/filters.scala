package edu.uwm.cs.gll.ast

trait Filter extends (Result[Node] => Boolean) { self =>
  
  def apply(res: Result[Node]) = res match {
    case Success(node, tail) => filter(node)
    case _ => true
  }
  
  def filter(n: Node): Boolean
  
  def unary_!(): Filter = new Filter {
    def filter(n: Node) = !self.filter(n)
  }
  
  def &(other: Filter): Filter = new Filter {
    def filter(n: Node) = self.filter(n) && other.filter(n)
  }
  
  def |(other: Filter): Filter = new Filter {
    def filter(n: Node) = self.filter(n) || other.filter(n)
  }
}

private[ast] class AssocFilter(sym: Symbol, isLeft: Boolean) extends Filter {
  def filter(n: Node): Boolean = n match {
    case n: BinaryNode if n.label == sym => 
      (if (isLeft) n.right.label else n.left.label) != sym
    
    case n => n.children forall filter
  }
}

object Filters {
  implicit def liftFilter[A <: Node](f: Filter)(str: Stream[Result[A]]): Stream[Result[A]] = str filter f
  
  implicit def symbolSyntax(sym: Symbol): RichSymbol = new RichSymbol(sym)
  
  class RichSymbol(sym: Symbol) {
    def <(): Filter = new AssocFilter(sym, true)
    
    def <>(): Filter = (this <) & (this >)
    
    def >(): Filter = new AssocFilter(sym, false)
  }
}
