package lambdacalc

import scala.io.Source
import scala.collection.mutable

import edu.uwm.cs.gll._

object LambdaCalcParser extends RegexParsers {
  import AST._
  
  // %%
  
  lazy val aliases = (alias+) ~ "-{3,}".r ~ expr ^^ { (a, _, e) => (a, e) }
                                
  lazy val alias = x ~ "=" ~ expr                ^^ { (id, _, e) => Alias(id, e) }
  
  lazy val expr: Parser[Expr] = (
      "\\" ~> x ~ "." ~ expr      ^^ { (id, _, e) => Lambda(id, e) }
    | exprNoLam
  )
  
  lazy val exprNoLam: Parser[Expr] = (
      exprNoLam ~ exprNoApp       ^^ App
    | exprNoApp
  )
  
  lazy val exprNoApp = (
      "(" ~> expr <~ ")"
    | x                           ^^ Val
  )
  
  val x = """[^\s\.\\()=;]+""".r  ^^ { id => Symbol(id) }
  
  // %%
  
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("=============================")
      
      val forest = aliases(LineStream(Source fromFile file))
      
      if (forest exists { _.isInstanceOf[Success[_]] }) {
        val errors = mutable.Set[String]()
        
        val status = for (Success((tree, expr), _) <- forest) yield {
          try {
            val env = tree.foldLeft(Map():Env) { (env, alias) =>
              env(alias.id) = alias.expr.eval(env)
            }
            
            Some(expr.eval(env))
          } catch {
            case e => {
              errors += e.getMessage
              None
            }
          }
        }
        
        val results = status flatMap { x => x }
        
        if (results.length == 0) {
          for (msg <- errors) {
            System.err.println("  runtime error: " + msg)
          }
        } else if (results.length == 1)
          println(results.head.format("  "))
        else
          System.err.printf("  parse error: Ambiguous parse: %s valid trees%n", results.length.toString)
      } else {
        val sorted = forest sort { _.tail.length < _.tail.length }
        val length = sorted.head.tail.length
        
        for (Failure(msg, tail) <- sorted takeWhile { _.tail.length == length }) {
          val pattern = "  parse error:%%d: %s%n    %%s%n    %%s%n".format(msg)
          tail.printError(pattern)(System.err)
        }
      }
      
      println()
    }
  }
}
