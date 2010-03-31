package miniml

import edu.uwm.cs.gll._

object MiniMLParser extends common.Example[Any] with RegexParsers {
  
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
  
  def parser = decs
  
  def handleSuccesses(v: Stream[Any]) {
    if (!v.isEmpty)
      println("  Successfully recognized!")
  }
}
