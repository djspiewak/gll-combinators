package lambdacalc

import scala.io.Source
import scala.collection.mutable

import edu.uwm.cs.gll._
import AST._

object LambdaCalcParser extends common.Example[(List[Alias], Expr)] with RegexParsers {
  
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
  
  def parser = aliases
  
  def handleSuccesses(forest: Stream[(List[Alias], Expr)]) {
    val errors = mutable.Set[String]()
        
    val status = for ((tree, expr) <- forest) yield {
      try {
        val env = tree.foldLeft(Map():Env) { (env, alias) =>
          env(alias.id) = alias.expr.eval(env)
        }
        
        Some(expr.eval(env))
      } catch {
        case EvalException(msg) => {
          errors += msg
          None
        }
        
        case e: StackOverflowError => {
          errors += "stack overflow"
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
      println("  " + results.head)
    else
      System.err.printf("  parse error: Ambiguous parse: %s valid trees%n", results.length.toString)
  }
}
