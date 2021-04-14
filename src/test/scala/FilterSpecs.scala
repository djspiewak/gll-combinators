/*
 * Copyright (c) 2021, Daniel Spiewak
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import com.codecommit.gll
import gll._
import gll.ast._

import scala.collection.compat.immutable.LazyList
import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

class FilterSpecs extends Specification
    with ScalaCheck
    with RegexParsers {

  import Prop._
  import Filters._

  "ast filtering" should {
    "disambiguate left-associativity" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | num                  ^^ IntLit
      ) filter prec(Add)

      expr("1") must beLike {
        case LazyList(Success(IntLit(1), LineStream())) => ok
      }

      expr("1 + 2") must beLike {
        case LazyList(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("1 + 2 + 3") must beLike {
        case LazyList(Success(Add(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }

      forAll { num: Int =>
        expr(1 to ((num % 15) + 1) mkString " + ").lengthCompare(1) mustEqual 0
      }
    }

    "disambiguate right-associativity" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ AddRight
        | num                  ^^ IntLit
      ) filter prec(AddRight)

      expr("1") must beLike {
        case LazyList(Success(IntLit(1), LineStream())) => ok
      }

      expr("1 + 2") must beLike {
        case LazyList(Success(AddRight(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("1 + 2 + 3") must beLike {
        case LazyList(Success(AddRight(IntLit(1), AddRight(IntLit(2), IntLit(3))), LineStream())) => ok
      }

      forAll { num: Int =>
        expr(1 to ((num % 15) + 1) mkString " + ").lengthCompare(1) mustEqual 0
      }
    }

    "disambiguate binary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | expr ~ ("-" ~> expr) ^^ Sub
        | num                  ^^ IntLit
      ) filter prec(Add, Sub)

      expr("1") must beLike {
        case LazyList(Success(IntLit(1), LineStream())) => ok
      }

      expr("1 + 2") must beLike {
        case LazyList(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("1 - 2") must beLike {
        case LazyList(Success(Sub(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("1 + 2 - 3") must beLike {
        case LazyList(Success(Sub(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }

      expr("1 - 2 + 3") must beLike {
        case LazyList(Success(Sub(IntLit(1), Add(IntLit(2), IntLit(3))), LineStream())) => ok
      }
    }

    "disambiguate unary and binary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | "-" ~> expr          ^^ Neg
        | num                  ^^ IntLit
      ) filter prec(Neg, Add)

      expr("1") must beLike {
        case LazyList(Success(IntLit(1), LineStream())) => ok
      }

      expr("-1") must beLike {
        case LazyList(Success(Neg(IntLit(1)), LineStream())) => ok
      }

      expr("1 + 2") must beLike {
        case LazyList(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("-1 + 2") must beLike {
        case LazyList(Success(Add(Neg(IntLit(1)), IntLit(2)), LineStream())) => ok
      }

      expr("1 + -2") must beLike {
        case LazyList(Success(Add(IntLit(1), Neg(IntLit(2))), LineStream())) => ok
      }
    }

    "disambiguate prefix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | "-" ~> expr          ^^ Neg
        | num                  ^^ IntLit
      ) filter prec(Add, Neg)

      expr("1") must beLike {
        case LazyList(Success(IntLit(1), LineStream())) => ok
      }

      expr("-1") must beLike {
        case LazyList(Success(Neg(IntLit(1)), LineStream())) => ok
      }

      expr("1 + 2") must beLike {
        case LazyList(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("-1 + 2") must beLike {
        case LazyList(Success(Neg(Add(IntLit(1), IntLit(2))), LineStream())) => ok
      }

      expr("1 + -2") must beLike {
        case LazyList(Success(Add(IntLit(1), Neg(IntLit(2))), LineStream())) => ok
      }
    }

    "disambiguate suffix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | expr <~ "~"          ^^ Comp
        | num                  ^^ IntLit
      ) filter prec(Add, Comp)

      expr("1") must beLike {
        case LazyList(Success(IntLit(1), LineStream())) => ok
      }

      expr("1~") must beLike {
        case LazyList(Success(Comp(IntLit(1)), LineStream())) => ok
      }

      expr("1 + 2") must beLike {
        case LazyList(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("1 + 2~") must beLike {
        case LazyList(Success(Comp(Add(IntLit(1), IntLit(2))), LineStream())) => ok
      }

      expr("1~ + 2") must beLike {
        case LazyList(Success(Add(Comp(IntLit(1)), IntLit(2)), LineStream())) => ok
      }
    }

    "ignore relative precedence of unary operations of the same fixity" in {
      lazy val expr: Parser[Expr] = (
          "-" ~> expr          ^^ Neg
        | "~" ~> expr          ^^ Comp2
        | "(" ~> expr <~ ")"
        | num                  ^^ IntLit
      ) filter prec(Add, Neg, Comp2)

      expr("-~1") must beLike {
        case LazyList(Success(Neg(Comp2(IntLit(1))), LineStream())) => ok
      }

      expr("~-1") must beLike {
        case LazyList(Success(Comp2(Neg(IntLit(1))), LineStream())) => ok
      }
    }

    "disambiguate non-uniform fixity unary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          "-" ~> expr          ^^ Neg
        | expr <~ "~"          ^^ Comp
        | num                  ^^ IntLit
      ) filter prec(Comp, Neg)

      expr("1") must beLike {
        case LazyList(Success(IntLit(1), LineStream())) => ok
      }

      expr("-1") must beLike {
        case LazyList(Success(Neg(IntLit(1)), LineStream())) => ok
      }

      expr("1~") must beLike {
        case LazyList(Success(Comp(IntLit(1)), LineStream())) => ok
      }

      expr("-1~") must beLike {
        case LazyList(Success(Neg(Comp(IntLit(1))), LineStream())) => ok
      }
    }

    "disambiguate uniform associativity operations with precedence levels" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr)       ^^ Add
        | expr ~ ("-" ~> expr)       ^^ Sub
        | expr ~ ("*" ~> expr)       ^^ Mul
        | num                        ^^ IntLit
      ) filter prec(Mul, (Add, Sub))

      expr("1 + 2") must beLike {
        case LazyList(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("1 - 2") must beLike {
        case LazyList(Success(Sub(IntLit(1), IntLit(2)), LineStream())) => ok
      }

      expr("1 + 2 - 3") must beLike {
        case LazyList(Success(Sub(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }

      expr("1 - 2 + 3") must beLike {
        case LazyList(Success(Add(Sub(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }

      expr("1 + 2 * 3") must beLike {
        case LazyList(Success(Add(IntLit(1), Mul(IntLit(2), IntLit(3))), LineStream())) => ok
      }
    }
  }

  // %%

  val num = """\d+""".r     ^^ { _.toInt }

  // %%

  sealed trait Expr extends Node {
    val solve: Int
  }

  case class Add(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = Symbol("add")

    val solve = left.solve + right.solve
  }

  case class AddRight(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = false
    override val assocRight = true

    val sym = Symbol("add")

    val solve = left.solve + right.solve
  }

  case class Sub(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = Symbol("sub")

    val solve = left.solve - right.solve
  }

  case class Mul(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = Symbol("mul")

    val solve = left.solve * right.solve
  }

  case class Div(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = Symbol("div")

    val solve = left.solve / right.solve
  }

  case class Neg(child: Expr) extends Expr with UnaryNode {
    val sym = Symbol("neg")

    val isPrefix = true

    val solve = -child.solve
  }

  case class Comp2(child: Expr) extends Expr with UnaryNode {
    val sym = Symbol("comp2")

    val isPrefix = true

    val solve = ~child.solve
  }

  case class Comp(child: Expr) extends Expr with UnaryNode {
    val sym = Symbol("comp")

    val isPrefix = false

    val solve = ~child.solve
  }

  case class IntLit(i: Int) extends Expr with LeafNode {
    val solve = i
  }
}
