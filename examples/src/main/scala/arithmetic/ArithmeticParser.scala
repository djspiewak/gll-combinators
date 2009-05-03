package arithmetic

import scala.io.Source

import edu.uwm.cs._
import gll.{RegexParsers, Success, Failure}
import util.StreamUtils

object ArithmeticParser extends RegexParsers {
  import StreamUtils._
  
  // %%
  
  lazy val expr: Parser[Expr] = (
      expr ~ "+" ~ term     ^^ { (e1, _, e2) => Add(e1, e2) }
    | expr ~ "-" ~ term     ^^ { (e1, _, e2) => Sub(e1, e2) }
    | term
  )
  
  lazy val term: Parser[Expr] = (
      term ~ "*" ~ factor   ^^ { (e1, _, e2) => Mul(e1, e2) }
    | term ~ "/" ~ factor   ^^ { (e1, _, e2) => Div(e1, e2) }
    | factor
  )
  
  lazy val factor: Parser[Expr] = (
      "(" ~> expr <~ ")"
    | "-" ~> factor           ^^ Neg
    | """\d+""".r           ^^ { str => IntLit(str.toInt) }
  )
  
  // %%
  
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("=============================")
      
      expr(readFile(file)) match {
        case Success(tree, _) :: Nil => println(tree)
        
        case errors => {
          val sorted = errors sort { _.tail.length < _.tail.length }
          val length = sorted.head.tail.length
          
          for (Failure(msg, _) <- sorted takeWhile { _.tail.length == length }) {
            println("  " + msg)
          }
        }
      }
      
      println()
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
    val complex: Boolean
    
    def format(tab: String): String
    
    override def toString = format("  ")
  }
  
  sealed trait BinExpr {
    val complex = true
    
    val e1: Expr
    val e2: Expr
    
    val sym: Char
    
    def format(tab: String) = {
      var back = tab + "(" + sym
      
      if (e1.complex || e2.complex) {
        back += '\n'
        back += e1.format(tab + "  ")
        
        back += '\n'
        back += e2.format(tab + "  ")
      } else {
        back += e1.format(" ")
        back += e2.format(" ")
      }
      
      back += ")"
      
      back
    }
  }
  
  case class Add(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '+'
    
    val solve = e1.solve + e2.solve
  }
  
  case class Sub(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '-'
    
    val solve = e1.solve - e2.solve
  }
  
  case class Mul(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '*'
    
    val solve = e1.solve * e2.solve
  }
  
  case class Div(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '/'
    
    val solve = e1.solve / e2.solve
  }
  
  case class Neg(e: Expr) extends Expr {
    val solve = -e.solve
    val complex = e.complex
    
    def format(tab: String) = {
      if (e.complex)
        tab + "(neg\n" + e.format(tab + "  ") + ")"
      else
        tab + "(neg" + e.format(" ") + ")"
    }
  }
  
  case class IntLit(i: Int) extends Expr {
    val solve = i
    val complex = false
    
    def format(tab: String) = tab + i
  }
}
