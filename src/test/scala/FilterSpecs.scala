import com.codecommit.gll
import gll._
import gll.ast._

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

object FilterSpecs extends Specification
    with NoTildeSyntax
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
        case Stream(Success(IntLit(1), LineStream())) => ok
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }
   
      expr("1 + 2 + 3") must beLike {
        case Stream(Success(Add(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }
      
      check { num: Int =>
        expr(1 to ((num % 15) + 1) mkString " + ").lengthCompare(1) mustEqual 0
      }
    }
    
    "disambiguate right-associativity" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ AddRight
        | num                  ^^ IntLit
      ) filter prec(AddRight)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => ok
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(AddRight(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("1 + 2 + 3") must beLike {
        case Stream(Success(AddRight(IntLit(1), AddRight(IntLit(2), IntLit(3))), LineStream())) => ok
      }
      
      check { num: Int =>
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
        case Stream(Success(IntLit(1), LineStream())) => ok
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("1 - 2") must beLike {
        case Stream(Success(Sub(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("1 + 2 - 3") must beLike {
        case Stream(Success(Sub(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }
      
      expr("1 - 2 + 3") must beLike {
        case Stream(Success(Sub(IntLit(1), Add(IntLit(2), IntLit(3))), LineStream())) => ok
      }
    }
    
    "disambiguate unary and binary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | "-" ~> expr          ^^ Neg
        | num                  ^^ IntLit
      ) filter prec(Neg, Add)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => ok
      }
      
      expr("-1") must beLike {
        case Stream(Success(Neg(IntLit(1)), LineStream())) => ok
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("-1 + 2") must beLike {
        case Stream(Success(Add(Neg(IntLit(1)), IntLit(2)), LineStream())) => ok
      }
      
      expr("1 + -2") must beLike {
        case Stream(Success(Add(IntLit(1), Neg(IntLit(2))), LineStream())) => ok
      }
    }
    
    "disambiguate prefix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | "-" ~> expr          ^^ Neg
        | num                  ^^ IntLit
      ) filter prec(Add, Neg)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => ok
      }
      
      expr("-1") must beLike {
        case Stream(Success(Neg(IntLit(1)), LineStream())) => ok
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("-1 + 2") must beLike {
        case Stream(Success(Neg(Add(IntLit(1), IntLit(2))), LineStream())) => ok
      }
      
      expr("1 + -2") must beLike {
        case Stream(Success(Add(IntLit(1), Neg(IntLit(2))), LineStream())) => ok
      }
    }
    
    "disambiguate suffix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | expr <~ "~"          ^^ Comp
        | num                  ^^ IntLit
      ) filter prec(Add, Comp)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => ok
      }
      
      expr("1~") must beLike {
        case Stream(Success(Comp(IntLit(1)), LineStream())) => ok
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("1 + 2~") must beLike {
        case Stream(Success(Comp(Add(IntLit(1), IntLit(2))), LineStream())) => ok
      }
      
      expr("1~ + 2") must beLike {
        case Stream(Success(Add(Comp(IntLit(1)), IntLit(2)), LineStream())) => ok
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
        case Stream(Success(Neg(Comp2(IntLit(1))), LineStream())) => ok
      }
      
      expr("~-1") must beLike {
        case Stream(Success(Comp2(Neg(IntLit(1))), LineStream())) => ok
      }
    }
    
    "disambiguate non-uniform fixity unary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          "-" ~> expr          ^^ Neg
        | expr <~ "~"          ^^ Comp
        | num                  ^^ IntLit
      ) filter prec(Comp, Neg)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => ok
      }
      
      expr("-1") must beLike {
        case Stream(Success(Neg(IntLit(1)), LineStream())) => ok
      }
      
      expr("1~") must beLike {
        case Stream(Success(Comp(IntLit(1)), LineStream())) => ok
      }
      
      expr("-1~") must beLike {
        case Stream(Success(Neg(Comp(IntLit(1))), LineStream())) => ok
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
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("1 - 2") must beLike {
        case Stream(Success(Sub(IntLit(1), IntLit(2)), LineStream())) => ok
      }
      
      expr("1 + 2 - 3") must beLike {
        case Stream(Success(Sub(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }
      
      expr("1 - 2 + 3") must beLike {
        case Stream(Success(Add(Sub(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => ok
      }
      
      expr("1 + 2 * 3") must beLike {
        case Stream(Success(Add(IntLit(1), Mul(IntLit(2), IntLit(3))), LineStream())) => ok
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
    val sym = 'add
    
    val solve = left.solve + right.solve
  }
  
  case class AddRight(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = false
    override val assocRight = true
    
    val sym = 'add
    
    val solve = left.solve + right.solve
  }
  
  case class Sub(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = 'sub
    
    val solve = left.solve - right.solve
  }
  
  case class Mul(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = 'mul
    
    val solve = left.solve * right.solve
  }
  
  case class Div(left: Expr, right: Expr) extends Expr with BinaryNode {
    val assocLeft = true
    val sym = 'div
    
    val solve = left.solve / right.solve
  }
  
  case class Neg(child: Expr) extends Expr with UnaryNode {
    val sym = 'neg
    
    val isPrefix = true
    
    val solve = -child.solve
  }
  
  case class Comp2(child: Expr) extends Expr with UnaryNode {
    val sym = 'comp2
    
    val isPrefix = true
    
    val solve = ~child.solve
  }
  
  case class Comp(child: Expr) extends Expr with UnaryNode {
    val sym = 'comp
    
    val isPrefix = false
    
    val solve = ~child.solve
  }
  
  case class IntLit(i: Int) extends Expr with LeafNode {
    val solve = i
  }
}
