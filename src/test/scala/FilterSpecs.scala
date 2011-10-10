import edu.uwm.cs.gll
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
    
    val solve = -child.solve
  }
  
  case class IntLit(i: Int) extends Expr {
    val label = 'int
    val children = Nil
    
    val solve = i
  }
}
