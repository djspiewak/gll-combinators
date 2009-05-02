package arithmetic

import scala.io.Source

import edu.uwm.cs._
import gll.{RegexParsers, Success, Failure}
import util.StreamUtils

object ArithmeticParser extends RegexParsers {
  import StreamUtils._
  
  // %%
  
  lazy val expr: Parser[Expr] = (
      expr ~ "*" ~ factor   ^^ { (e1, _, e2) => Mul(e1, e2) }
    | expr ~ "/" ~ factor   ^^ { (e1, _, e2) => Div(e1, e2) }
    | factor
  )
  
  lazy val factor: Parser[Expr] = (
      factor ~ "+" ~ term   ^^ { (e1, _, e2) => Add(e1, e2) }
    | factor ~ "-" ~ term   ^^ { (e1, _, e2) => Sub(e1, e2) }
    | term
  )
  
  lazy val term: Parser[Expr] = (
      "(" ~> expr <~ ")"
    | "-" ~> term           ^^ Neg
    | """\d+""".r           ^^ { str => IntLit(str.toInt) }
  )
  
  // %%
  
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("=============================")
      
      expr(readFile(file)) match {
        case Success(tree, _) :: Nil => println("  " + tree)
        
        case errors => {
          val sorted = errors sort { _.tail.length < _.tail.length }
          val length = sorted.head.tail.length
          
          for (Failure(msg, _) <- sorted takeWhile { _.tail.length == length }) {
            println("  " + msg)
          }
        }
      }
    }
  }
  
  private def readFile(file: String) = {
    val source = Source.fromFile(file)
    
    def gen(): Stream[Char] = {
      if (source.hasNext)
        source.next #:: gen()
      else
        Stream.empty
    }
    
    gen()
  }
  
  // AST
  
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
