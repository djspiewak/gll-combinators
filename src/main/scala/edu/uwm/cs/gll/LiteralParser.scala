package edu.uwm.cs.gll

import StreamUtils._

case class LiteralParser[+R](str: String) extends TerminalParser[String] {
  def computeFirst(s: Set[Parser[Any]]) = {
    if (str.length > 0) Set(str(0))
    else Set()
  }
  
  def apply(in: Stream[Char]) = {
    val trunc = in take str.length
    lazy val errorMessage = "Expected '%s' got '%s'".format(str, trunc.mkString)
    
    List(if (trunc.lengthCompare(str.length) != 0) {
      Failure(errorMessage, in)
    } else {
      val succ = trunc.zipWithIndex forall {
        case (c, i) => c == str(i)
      }
      
      if (succ) 
        Success(str, in drop str.length)
      else 
        Failure(errorMessage, in)
    })
  }
  
  override def equals(other: Any) = other match {
    case that: LiteralParser[R] => this.str == that.str
    case _ => false
  }
  
  override def hashCode = str.hashCode
}
