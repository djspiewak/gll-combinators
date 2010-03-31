package edu.uwm.cs.gll.ast

trait Filter extends (Node => Boolean) { self =>
  
  def apply(n: Node): Boolean
  
  def unary_!(): Filter = new Filter {
    def apply(n: Node) = !self(n)
  }
  
  def &(other: Filter): Filter = new Filter {
    def apply(n: Node) = self(n) && other(n)
  }
  
  def |(other: Filter): Filter = new Filter {
    def apply(n: Node) = self(n) || other(n)
  }
}

private[ast] class AssocFilter(sym: Symbol, isLeft: Boolean) extends Filter {
  def apply(n: Node): Boolean = n match {
    case n: BinaryNode if n.label == sym => 
      (if (isLeft) n.right.label else n.left.label) != sym
    
    case n => n.children forall apply
  }
}

private[ast] class PrecedenceFilter(order: Seq[Symbol]) extends Filter {
  val forbidden = {
    val sets = for (i <- 0 until order.length) 
      yield (order(i) -> Set(order drop (i + 1): _*))
    
    Map(sets: _*)
  }
  
  def apply(n: Node): Boolean = {
    val valid = if (forbidden contains n.label)
      !(n.children map { _.label } exists forbidden(n.label))
    else
      true
    
    (n.children forall apply) && valid
  }
}

object Filters {
  def prec(order: Symbol*): Filter = new PrecedenceFilter(order)
  
  implicit def liftFilter[A <: Node](f: Filter)(str: Stream[Result[A]]): Stream[Result[A]] = {
    str filter {
      case Success(node, _) => f(node)
      case _ => true
    }
  }
  
  implicit def liftPredicate(pred: Node => Boolean): Filter = new Filter {
    def apply(n: Node) = pred(n)
  }
  
  implicit def symbolSyntax(sym: Symbol): RichSymbol = new RichSymbol(sym)
  
  class RichSymbol(sym: Symbol) {
    def <(): Filter = new AssocFilter(sym, true)
    
    def <>(): Filter = (this <) & (this >)
    
    def >(): Filter = new AssocFilter(sym, false)
  }
}
