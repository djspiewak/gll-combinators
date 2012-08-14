import com.codecommit.gll._

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

object ArithmeticSpecs extends Specification with ScalaCheck with RegexParsers {
  import Prop._
  import StreamUtils._
  
  "arithmetic grammar" should {
    "compute FIRST set" in {
      val first = expr.first
      
      if (first.size != 0) {
        forall(Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-')) { c =>
          first must contain(c)
        }
      } else
        first mustEqual Set()
    }
    
    "parse numbers" in check { x: Int =>
      (abs(x.toLong) < Math.MAX_INT) ==> {
        expr(x.toString) must beLike {
          case Success(e, LineStream()) #:: SNil => e.solve mustEqual x
        }
      }
    }
    
    "parse simple addition" in check { (x: Int, y: Int) =>
      (abs(x.toLong) < Math.MAX_INT && abs(y.toLong) < Math.MAX_INT) ==> {
        val res = expr((x + "+" + y))
        
        if (x < 0) {
          res must haveSize(2)
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Add(Neg(e1), e2), LineStream()) =>
                e.solve mustEqual x + y
            }
          }
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Neg(Add(e1, e2)), LineStream()) =>
                e.solve mustEqual -(-x + y)
            }
          }
        } else {
          res must beLike {
            case Success(e, LineStream()) #:: SNil =>
              e.solve mustEqual x + y
          }
        }
      }
    }
    
    "parse simple subtraction" in check { (x: Int, y: Int) =>
      (abs(x.toLong) < Math.MAX_INT && abs(y.toLong) < Math.MAX_INT) ==> {
        val res = expr((x + "-" + y))
        
        if (x < 0) {
          res.length mustEqual 2
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Sub(Neg(e1), e2), LineStream()) =>
                e.solve mustEqual x - y
            }
          }
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Neg(Sub(e1, e2)), LineStream()) =>
                e.solve mustEqual -(-x - y)
            }
          }
        } else {
          res must beLike {
            case Success(e, LineStream()) #:: SNil =>
              e.solve mustEqual x - y
          }
        }
      }
    }
    
    "parse simple multiplication" in check { (x: Int, y: Int) =>
      (abs(x.toLong) < Math.MAX_INT && abs(y.toLong) < Math.MAX_INT) ==> {
        val res = expr((x + "*" + y))
        
        if (x < 0) {
          res.length mustEqual 2
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Mul(Neg(e1), e2), LineStream()) =>
                e.solve mustEqual x * y
            }
          }
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Neg(Mul(e1, e2)), LineStream()) =>
                e.solve mustEqual -(-x * y)
            }
          }
        } else {
          res must beLike {
            case Success(e, LineStream()) #:: SNil =>
              e.solve mustEqual x * y
          }
        }
      }
    }
    
    "parse simple division" in check { (x: Int, y: Int) =>
      (abs(x.toLong) < Math.MAX_INT && abs(y.toLong) < Math.MAX_INT && y != 0) ==> {
        val res = expr((x + "/" + y))
        
        if (x < 0) {
          res.length mustEqual 2
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Div(Neg(e1), e2), LineStream()) =>
                e.solve mustEqual x / y
            }
          }
          
          atLeastOnce(res) { r =>
            r must beLike {
              case Success(e @ Neg(Div(e1, e2)), LineStream()) =>
                e.solve mustEqual -(-x / y)
            }
          }
        } else {
          res must beLike {
            case Success(e, LineStream()) #:: SNil =>
              e.solve mustEqual x / y
          }
        }
      }
    }
    
    "produce both associativity configurations" in {
      val res = {
        val back = expr("42 + 13 + 12")
        
        forall(back) { r =>
          r must beLike {
            case Success(e, LineStream()) => ok
          }
        }
        
        back collect {
          case Success(e, LineStream()) => e
        }
      }
      
      val target = Set(Add(IntLit(42), Add(IntLit(13), IntLit(12))),
                       Add(Add(IntLit(42), IntLit(13)), IntLit(12)))
      
      Set(res: _*) mustEqual target
    }
    
    "produce both binary precedence configurations" in {
      val res = {
        val back = expr("42 + 13 - 12")
        
        forall(back) { r =>
          r must beLike {
            case Success(e, LineStream()) => ok
          }
        }
        
        back collect { 
          case Success(e, LineStream()) => e
        }
      }
        
      val target = Set(Add(IntLit(42), Sub(IntLit(13), IntLit(12))),
                       Sub(Add(IntLit(42), IntLit(13)), IntLit(12)))
      
      Set(res: _*) mustEqual target
    }
    
    "produce both unary precedence configurations" in {
      val res = {
        val back = expr("-42 + 13")
        
        forall(back) { r =>
          r must beLike {
            case Success(e, LineStream()) => ok
          }
        }
        
        back collect {
          case Success(e, LineStream()) => e.solve
        }
      }
      
      (res sort { _ < _ } toList) mustEqual List(-55, -29)
    }
  }
  
  def abs(i: Int) = Math.abs(i)
  
  def abs(l: Long) = Math.abs(l)
  
  // %%
  
  lazy val expr: Parser[Expr] = (
      expr ~ ("+" ~> expr)  ^^ Add
    | expr ~ ("-" ~> expr)  ^^ Sub
    | expr ~ ("*" ~> expr)  ^^ Mul
    | expr ~ ("/" ~> expr)  ^^ Div
    | "-" ~> expr           ^^ Neg
    | num                   ^^ IntLit
  )
  
  val num = """\d+""".r     ^^ { _.toInt }
  
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
