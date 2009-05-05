package miniml

import scala.io.Source
import edu.uwm.cs.gll._

object MiniMLParser extends RegexParsers {
  
  override val whitespace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r
  
  // %%
  
  lazy val decs = dec*
  
  lazy val dec = (
      "val" ~ pat ~ "=" ~ exp
    | "val" ~ "rec" ~ x ~ "=" ~ "fn" ~ matcher
  ) ^^^ null
  
  lazy val exp: Parser[Any] = (
      x
    | b
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp
    | n
    | exp ~ op ~ exp
    
    | "(" ~ ")"
    | "(" ~ commaExps ~ ")"
    | "fn" ~ matcher
    | exp ~ exp
    
    | "nil"
    | exp ~ "::" ~ exp
    | "case" ~ exp ~ "of" ~ matcher
    | "let" ~ decs ~ "in" ~ exp ~ "end"
  ) ^^^ null
  
  lazy val commaExps: Parser[Any] = (
      exp
    | commaExps ~ "," ~ exp
  )
  
  lazy val matcher: Parser[Any] = (
      mrule
    | mrule ~ "|" ~ matcher
  ) ^^^ null
  
  lazy val mrule = pat ~ "=>" ~ exp ^^^ null
          
  lazy val pat: Parser[Any] = (
      "_"
    | x
    | b
    | n
    | "(" ~ ")"
    | "(" ~ commaPats ~ ")"
    | "nil"
    | pat ~ "::" ~ pat
  ) ^^^ null
  
  lazy val commaPats: Parser[Any] = (
      pat
    | commaPats ~ "," ~ pat
  ) ^^^ null
  
  val x = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  
  val b = "true" | "false"
  
  val n = """~?\d+"""r
  
  val op = (
      "=" | "<" | ">" | "<=" | ">=" 
    | "+" | "-" | "*" | "div" | "mod" 
    | "@" | "o" | "andalso" | "orelse"
  )
  
  // %%
  
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("=============================")
      
      decs(LineStream(Source fromFile file)) match {
        case Success(tree, _) :: _ => println("  Successfully recognized!")
        
        case errors => {
          val sorted = errors sort { _.tail.length < _.tail.length }
          val length = sorted.head.tail.length
          
          for (Failure(msg, tail) <- sorted takeWhile { _.tail.length == length }) {
            val pattern = "%s:%%d: %s%n  %%s%n  %%s%n".format(file, msg)
            tail.printError(pattern)(System.err)
          }
        }
      }
      
      println()
    }
  }
}
