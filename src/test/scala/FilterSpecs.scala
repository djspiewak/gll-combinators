import com.codecommit.gll
import gll._
import gll.ast._

import org.specs._
import org.scalacheck._

object FilterSpecs extends Specification with ScalaCheck with RegexParsers {
  import Prop._
  import Filters._
  
  "ast filtering" should {
    "disambiguate left-associativity" in {
      val assoc = 'add <
      
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | num                  ^^ IntLit
      ) filter assoc
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2 + 3") must beLike {
        case Stream(Success(Add(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => true
        case _ => false
      }
      
      val prop = forAll { num: Int =>
        expr(1 to ((num % 15) + 1) mkString " + ").lengthCompare(1) == 0
      }
      
      prop must pass
    }
    
    "disambiguate right-associativity" in {
      val assoc = 'add >
      
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | num                  ^^ IntLit
      ) filter assoc
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2 + 3") must beLike {
        case Stream(Success(Add(IntLit(1), Add(IntLit(2), IntLit(3))), LineStream())) => true
        case _ => false
      }
      
      val prop = forAll { num: Int =>
        expr(1 to ((num % 15) + 1) mkString " + ").lengthCompare(1) == 0
      }
      
      prop must pass
    }
    
    "disambiguate binary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | expr ~ ("-" ~> expr) ^^ Sub
        | num                  ^^ IntLit
      ) filter prec('add, 'sub)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("1 - 2") must beLike {
        case Stream(Success(Sub(IntLit(1), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2 - 3") must beLike {
        case Stream(Success(Sub(Add(IntLit(1), IntLit(2)), IntLit(3)), LineStream())) => true
        case _ => false
      }
      
      expr("1 - 2 + 3") must beLike {
        case Stream(Success(Sub(IntLit(1), Add(IntLit(2), IntLit(3))), LineStream())) => true
        case _ => false
      }
    }
    
    "disambiguate unary and binary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | "-" ~> expr          ^^ Neg
        | num                  ^^ IntLit
      ) filter prec('neg, 'add)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => true
        case _ => false
      }
      
      expr("-1") must beLike {
        case Stream(Success(Neg(IntLit(1)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("-1 + 2") must beLike {
        case Stream(Success(Add(Neg(IntLit(1)), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + -2") must beLike {
        case Stream(Success(Add(IntLit(1), Neg(IntLit(2))), LineStream())) => true
        case _ => false
      }
    }
    
    "disambiguate prefix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | "-" ~> expr          ^^ Neg
        | num                  ^^ IntLit
      ) filter prec('add, 'neg)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => true
        case _ => false
      }
      
      expr("-1") must beLike {
        case Stream(Success(Neg(IntLit(1)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("-1 + 2") must beLike {
        case Stream(Success(Neg(Add(IntLit(1), IntLit(2))), LineStream())) => true
        case _ => false
      }
      
      expr("1 + -2") must beLike {
        case Stream(Success(Add(IntLit(1), Neg(IntLit(2))), LineStream())) => true
        case _ => false
      }
    }
    
    "disambiguate suffix unary and binary operations when binary has higher precedence" in {
      lazy val expr: Parser[Expr] = (
          expr ~ ("+" ~> expr) ^^ Add
        | expr <~ "~"          ^^ Comp
        | num                  ^^ IntLit
      ) filter prec('add, 'comp)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => true
        case _ => false
      }
      
      expr("1~") must beLike {
        case Stream(Success(Comp(IntLit(1)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2") must beLike {
        case Stream(Success(Add(IntLit(1), IntLit(2)), LineStream())) => true
        case _ => false
      }
      
      expr("1 + 2~") must beLike {
        case Stream(Success(Comp(Add(IntLit(1), IntLit(2))), LineStream())) => true
        case _ => false
      }
      
      expr("1~ + 2") must beLike {
        case Stream(Success(Add(Comp(IntLit(1)), IntLit(2)), LineStream())) => true
        case _ => false
      }
    }
    
    "ignore relative precedence of unary operations of the same fixity" in {
      lazy val expr: Parser[Expr] = (
          "-" ~> expr          ^^ Neg
        | "~" ~> expr          ^^ Comp2
        | "(" ~> expr <~ ")"
        | num                  ^^ IntLit
      ) filter prec('add, 'neg, 'comp2)
      
      expr("-~1") must beLike {
        case Stream(Success(Neg(Comp2(IntLit(1))), LineStream())) => true
        case _ => false
      }
      
      expr("~-1") must beLike {
        case Stream(Success(Comp2(Neg(IntLit(1))), LineStream())) => true
        case _ => false
      }
    }
    
    "disambiguate non-uniform fixity unary operations with precedence" in {
      lazy val expr: Parser[Expr] = (
          "-" ~> expr          ^^ Neg
        | expr <~ "~"          ^^ Comp
        | num                  ^^ IntLit
      ) filter prec('comp, 'neg)
      
      expr("1") must beLike {
        case Stream(Success(IntLit(1), LineStream())) => true
        case _ => false
      }
      
      expr("-1") must beLike {
        case Stream(Success(Neg(IntLit(1)), LineStream())) => true
        case _ => false
      }
      
      expr("1~") must beLike {
        case Stream(Success(Comp(IntLit(1)), LineStream())) => true
        case _ => false
      }
      
      expr("-1~") must beLike {
        case Stream(Success(Neg(Comp(IntLit(1))), LineStream())) => true
        case _ => false
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
    val label = 'add
    
    val solve = left.solve + right.solve
  }
  
  case class Sub(left: Expr, right: Expr) extends Expr with BinaryNode {
    val label = 'sub
    
    val solve = left.solve - right.solve
  }
  
  case class Mul(left: Expr, right: Expr) extends Expr with BinaryNode {
    val label = 'mul
    
    val solve = left.solve * right.solve
  }
  
  case class Div(left: Expr, right: Expr) extends Expr with BinaryNode {
    val label = 'div
    
    val solve = left.solve / right.solve
  }
  
  case class Neg(child: Expr) extends Expr with UnaryNode {
    val label = 'neg
    
    val isPrefix = true
    
    val solve = -child.solve
  }
  
  case class Comp2(child: Expr) extends Expr with UnaryNode {
    val label = 'comp2
    
    val isPrefix = true
    
    val solve = ~child.solve
  }
  
  case class Comp(child: Expr) extends Expr with UnaryNode {
    val label = 'comp
    
    val isPrefix = false
    
    val solve = ~child.solve
  }
  
  case class IntLit(i: Int) extends Expr {
    val label = 'int
    val children = Nil
    
    val solve = i
  }
}
