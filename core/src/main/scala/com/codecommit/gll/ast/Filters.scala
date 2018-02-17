package com.codecommit.gll.ast

import com.codecommit.gll._

trait Filters {
  import Filters.ClassName

  def prec(levels: PrecLevel*): Filter[Node] = new Filter[Node] {
    private val normalized: Seq[Set[ClassName]] = levels.map(_.specs.map(s => ClassName(s.m.runtimeClass.getName)))

    private val dag: Map[ClassName, Set[ClassName]] = {
      val (back, _) = normalized.foldRight((Map[ClassName, Set[ClassName]](), Set[ClassName]())) {
        case (level, (dag, acc)) =>
          (dag ++ (level map { _ -> acc }), acc ++ level)
      }

      back
    }

    private val matched: Map[ClassName, Set[ClassName]] = {
      val pairs = normalized flatMap { level =>
        level map { m => m -> level }
      }

      Map(pairs: _*)
    }

    def apply(node: Node): Boolean = {
      import FormSpec._

      val form = node.form.linearize
      val cname = ClassName(node.getClass.getName)

      val forbidden = dag.get(cname).getOrElse(Set())
      val peers = matched.get(cname).getOrElse(Set())

      (form.head, form drop 1 take (form.length - 2), form.last) match {
        case (HolePart(left), middle, HolePart(right)) => {
          val leftCName = ClassName(left.getClass.getName)
          val rightCName = ClassName(right.getClass.getName)

          lazy val leftBeforeRight = (node.children indexOf left) < (node.children indexOf right)

          lazy val leftAssoc =
            if (peers contains leftCName) leftBeforeRight else true

          lazy val rightAssoc =
            if (peers contains rightCName) !leftBeforeRight else true

          lazy val checkLeft = left.form.linearize.last match {
            case HolePart(_) => !(forbidden contains leftCName)
            case _ => true
          }

          lazy val checkRight = right.form.linearize.head match {
            case HolePart(_) => !(forbidden contains rightCName)
            case _ => true
          }

          leftAssoc && rightAssoc && checkLeft && checkRight
        }

        // prefix unary
        case (head, middle, HolePart(child)) if head.isSimple && (middle.lastOption map { _.isSimple } getOrElse true) => {
          val childCName = ClassName(child.getClass.getName)

          child.form.linearize.head match {
            case HolePart(_) => !(forbidden contains childCName)
            case _ => true
          }
        }

        // suffix unary
        case (HolePart(child), middle, last) if last.isSimple && (middle.headOption map { _.isSimple } getOrElse true) => {
          val childCName = ClassName(child.getClass.getName)

          child.form.linearize.last match {
            case HolePart(_) => !(forbidden contains childCName)
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
}

object Filters extends Filters {
  private final case class ClassName(name: String) extends AnyVal
}
