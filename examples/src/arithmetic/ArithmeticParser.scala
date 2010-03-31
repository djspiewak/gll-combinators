package arithmetic

import edu.uwm.cs.gll._
import AST._

object ArithmeticParser extends common.Example[Expr] with RegexParsers {
  
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
    | "-" ~> factor         ^^ Neg
    | """\d+""".r           ^^ { str => IntLit(str.toInt) }
  )
  
  // %%
  
  def parser = expr
  
  def handleSuccesses(forest: Stream[Expr]) {
    forest foreach println
  }
}
