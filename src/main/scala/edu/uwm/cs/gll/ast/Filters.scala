package edu.uwm.cs.gll.ast

trait Filters {
  def prec(order: Symbol*): Filter[Node] = new PrecedenceFilter(order)
  
  implicit def liftFilter[A](f: Filter[A])(str: Stream[Result[A]]): Stream[Result[A]] = {
    str filter {
      case Success(node, _) => f(node)
      case _ => true
    }
  }
  
  implicit def liftPredicate[A](pred: A => Boolean): Filter[A] = new Filter[A] {
    def apply(n: A) = pred(n)
  }
  
  implicit def symbolSyntax(sym: Symbol): RichSymbol = new RichSymbol(sym)
  
  class RichSymbol(sym: Symbol) {
    def <(): Filter[Node] = new AssocFilter(sym, true)
    
    def <>(): Filter[Node] = (this <) & (this >)
    
    def >(): Filter[Node] = new AssocFilter(sym, false)
  }
  
  private class AssocFilter(sym: Symbol, isLeft: Boolean) extends Filter[Node] {
    def apply(n: Node): Boolean = n match {
      case n: BinaryNode if n.label == sym => {
        val valid = (if (isLeft) n.right.label else n.left.label) != sym
        valid && (n.children forall apply)
      }
      
      case n => n.children forall apply
    }
  }
  
  private class PrecedenceFilter(order: Seq[Symbol]) extends Filter[Node] {
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
      
      valid && (n.children forall apply)
    }
  }
}

object Filters extends Filters
