package com.codecommit.gll.ast

import com.codecommit.gll._

trait Filters {
  def prec(levels: PrecLevel*): Filter[Node] = new Filter[Node] {
    private val normalized: Seq[Set[Manifest[_]]] = levels map { l => l.specs map { _.m } }
    
    private val dag: Map[Manifest[_], Set[Manifest[_]]] = { 
      val (back, _) = normalized.foldRight((Map[Manifest[_], Set[Manifest[_]]](), Set[Manifest[_]]())) {
        case (level, (dag, acc)) =>
          (dag ++ (level map { _ -> acc }), acc ++ level)
      }
      
      back
    }
    
    private val matched: Map[Manifest[_], Set[Manifest[_]]] = {
      val pairs = normalized flatMap { level =>
        level map { m => m -> level }
      }
      
      Map(pairs: _*)
    }
    
    def apply(node: Node): Boolean = {
      import FormSpec._
      
      val form = node.form.linearize
      val manifest = Manifest.classType(node.getClass)
      
      val forbidden = dag get manifest getOrElse Set()
      val peers = matched get manifest getOrElse Set()
      
      (form.head, form drop 1 take (form.length - 2), form.last) match {
        case (HolePart(left), middle, HolePart(right)) if middle forall { _.isSimple } => {
          val leftManifest = Manifest.classType(left.getClass)
          val rightManifest = Manifest.classType(right.getClass)
          
          lazy val leftBeforeRight = (node.children indexOf left) < (node.children indexOf right)
          
          lazy val leftAssoc = 
            if (peers contains leftManifest) leftBeforeRight else true
          
          lazy val rightAssoc =
            if (peers contains rightManifest) !leftBeforeRight else true
          
          lazy val checkLeft = left.form.linearize.last match {
            case HolePart(_) => !(forbidden contains leftManifest)
            case _ => true
          }
          
          lazy val checkRight = right.form.linearize.head match {
            case HolePart(_) => !(forbidden contains rightManifest)
            case _ => true
          }
          
          leftAssoc && rightAssoc && checkLeft && checkRight
        }
        
        // prefix unary
        case (head, middle, HolePart(child)) if head.isSimple && (middle forall { _.isSimple }) => {
          val childManifest = Manifest.classType(child.getClass)
          
          child.form.linearize.head match {
            case HolePart(_) => !(forbidden contains childManifest)
            case _ => true
          }
        }
        
        // suffix unary
        case (HolePart(child), middle, last) if last.isSimple && (middle forall { _.isSimple }) => {
          val childManifest = Manifest.classType(child.getClass)
          
          child.form.linearize.last match {
            case HolePart(_) => !(forbidden contains childManifest)
            case _ => true
          }
        }
        
        // TODO additional forms
        
        case _ => true
      }
      
      // TODO additional checks
    }
  }
  
  
  case class PrecLevel(specs: Set[ManWrap])
  
  case object PrecLevel extends (Set[ManWrap] => PrecLevel) {
    implicit def coerce1[A <% ManWrap](a: A): PrecLevel =
      PrecLevel(Set[ManWrap](a))
    
    implicit def coerce2[A <% ManWrap, B <% ManWrap](pair: (A, B)): PrecLevel =
      PrecLevel(Set[ManWrap](pair._1, pair._2))
    
    implicit def coerce3[A <% ManWrap, B <% ManWrap, C <% ManWrap](pair: (A, B, C)): PrecLevel =
      PrecLevel(Set[ManWrap](pair._1, pair._2, pair._3))
    
    implicit def coerce4[A <% ManWrap, B <% ManWrap, C <% ManWrap, D <% ManWrap](pair: (A, B, C, D)): PrecLevel =
      PrecLevel(Set[ManWrap](pair._1, pair._2, pair._3, pair._4))
    
    implicit def coerce5[A <% ManWrap, B <% ManWrap, C <% ManWrap, D <% ManWrap, E <% ManWrap](pair: (A, B, C, D, E)): PrecLevel =
      PrecLevel(Set[ManWrap](pair._1, pair._2, pair._3, pair._4, pair._5))
  }
  
  // newtype ManWrap { m :: Manifest }
  case class ManWrap(m: Manifest[_])
  
  case object ManWrap extends (Manifest[_] => ManWrap) {
    implicit def coerceValue0[A <: Node](v: A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion1[A <: Node](c: _ => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion2[A <: Node](c: (_, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion3[A <: Node](c: (_, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion4[A <: Node](c: (_, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion5[A <: Node](c: (_, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion6[A <: Node](c: (_, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion7[A <: Node](c: (_, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion8[A <: Node](c: (_, _, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion9[A <: Node](c: (_, _, _, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
    
    implicit def coerceCompanion10[A <: Node](c: (_, _, _, _, _, _, _, _, _, _) => A)(implicit m: Manifest[A]): ManWrap =
      ManWrap(m)
  }
  
  /* def prec(order: Symbol*): Filter[Node] = new PrecedenceFilter(order)
  
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
      case n: BinaryNode if n.label == sym =>
        (if (isLeft) n.right.label else n.left.label) != sym
      
      case n => true
    }
  }
  
  private class PrecedenceFilter(order: Seq[Symbol]) extends Filter[Node] {
    val forbidden = {
      val sets = for (i <- 0 until order.length) 
        yield (order(i) -> Set(order drop (i + 1): _*))
      
      Map(sets: _*)
    }
    
    def apply(n: Node): Boolean = {
      if (forbidden contains n.label) {
        lazy val fallback = !(n.children map { _.label } exists forbidden(n.label))
        
        n match {
          case bn: BinaryNode => {
            lazy val leftCheck = bn.left match {
              case un: UnaryNode if !un.isPrefix =>
                true
              
              case n => !forbidden(bn.label)(n.label)
            }
            
            lazy val rightCheck = bn.right match {
              case un: UnaryNode if un.isPrefix =>
                true
             
              case n => !forbidden(bn.label)(n.label) 
            }
            
            leftCheck && rightCheck
          }
          
          case un: UnaryNode => {
            un.child match {
              case un2: UnaryNode if un2.isPrefix == un.isPrefix =>
                true
              
              case _ => fallback
            }
          }
          
          case _ => fallback
        }
      } else {
        true
      }
    }
  } */
}

object Filters extends Filters
