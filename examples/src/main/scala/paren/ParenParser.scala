package paren

object ParenParser extends common.Example[Int] {
  
  // %%
  
  lazy val expr: Parser[Int] = (
      "(" ~> expr <~ ")"  ^^ { _ + 1 }
    | "()"               ^^^ 1
  )
  
  // %%
  
  def parser = expr
  
  def handleSuccesses(forest: List[Int]) {
    for (depth <- forest) {
      println("  " + depth)
    }
  }
}
