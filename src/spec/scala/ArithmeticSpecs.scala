import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object ArithmeticSpecs extends Specification with ScalaCheck with ImplicitConversions {
  import Prop._
  import StreamUtils._
  
  "arithmetic grammar" should {
    "parse numbers" in {
      val prop = forAll { x: Int =>
        expr(x.toString.toProperStream) match {
          case Success(e, Stream()) :: Nil => e.solve == x
          case _ => false
        }
      }
      
      prop must pass
    }
    
    "parse simple addition" in {
      val prop = forAll { (x: Int, y: Int) =>
        val res = expr((x + "+" + y) toProperStream)
        
        if (x < 0) {
          res.length == 2 && res.forall {
            case Success(e @ Add(Neg(e1), e2), Stream()) => e.solve == x + y
            case Success(e @ Neg(Add(e1, e2)), Stream()) => e.solve == -(-x + y)
            case _ => false
          }
        } else {
          res match {
            case Success(e, Stream()) :: Nil => e.solve == x + y
            case _ => false
          }
        }
      }
      
      prop must pass
    }
    
    "parse simple subtraction" in {
      val prop = forAll { (x: Int, y: Int) =>
        val res = expr((x + "-" + y) toProperStream)
        
        if (x < 0) {
          res.length == 2 && res.forall {
            case Success(e @ Sub(Neg(e1), e2), Stream()) => e.solve == x - y
            case Success(e @ Neg(Sub(e1, e2)), Stream()) => e.solve == -(-x - y)
            case _ => false
          }
        } else {
          res match {
            case Success(e, Stream()) :: Nil => e.solve == x - y
            case _ => false
          }
        }
      }
      
      prop must pass
    }
    
    "parse simple multiplication" in {
      val prop = forAll { (x: Int, y: Int) =>
        val res = expr((x + "*" + y) toProperStream)
        
        if (x < 0) {
          res.length == 2 && res.forall {
            case Success(e @ Mul(Neg(e1), e2), Stream()) => e.solve == x * y
            case Success(e @ Neg(Mul(e1, e2)), Stream()) => e.solve == -(-x * y)
            case _ => false
          }
        } else {
          res match {
            case Success(e, Stream()) :: Nil => e.solve == x * y
            case _ => false
          }
        }
      }
      
      prop must pass
    }
    
    "parse simple division" in {
      val prop = forAll { (x: Int, y: Int) =>
        y != 0 ==> {
          val res = expr((x + "/" + y) toProperStream)
          
          if (x < 0) {
            res.length == 2 && res.forall {
              case Success(e @ Div(Neg(e1), e2), Stream()) => e.solve == x / y
              case Success(e @ Neg(Div(e1, e2)), Stream()) => e.solve == -(-x / y)
              case _ => false
            }
          } else {
            res match {
              case Success(e, Stream()) :: Nil => e.solve == x / y
              case _ => false
            }
          }
        }
      }
      
      prop must pass
    }
  }
  
  // %%
  
  lazy val expr: Parser[Expr] = (
      expr ~ "+" ~ expr   ^^ { case e1 ~ _ ~ e2 => Add(e1, e2) }
    | expr ~ "-" ~ expr   ^^ { case e1 ~ _ ~ e2 => Sub(e1, e2) }
    | expr ~ "*" ~ expr   ^^ { case e1 ~ _ ~ e2 => Mul(e1, e2) }
    | expr ~ "/" ~ expr   ^^ { case e1 ~ _ ~ e2 => Div(e1, e2) }
    | "-" ~> expr         ^^ Neg
    | num                 ^^ IntLit
  )
  
  lazy val num: Parser[Int] = (
      num ~ digit ^^ { case n ~ d => (n * 10) + d }
    | digit
  )
  
  val digit = ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") ^^ { _.toInt }
  
  // %%
  
  sealed trait Expr {
    val solve: Int
  }
  
  case class Add(e1: Expr, e2: Expr) extends Expr {
    val solve = e1.solve + e2.solve
  }
  
  case class Sub(e1: Expr, e2: Expr) extends Expr {
    val solve = e1.solve - e2.solve
  }
  
  case class Mul(e1: Expr, e2: Expr) extends Expr {
    val solve = e1.solve * e2.solve
  }
  
  case class Div(e1: Expr, e2: Expr) extends Expr {
    val solve = e1.solve / e2.solve
  }
  
  case class Neg(e: Expr) extends Expr {
    val solve = -e.solve
  }
  
  case class IntLit(i: Int) extends Expr {
    val solve = i
  }
}
