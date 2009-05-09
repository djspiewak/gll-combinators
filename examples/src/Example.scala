package common

import scala.io.Source
import edu.uwm.cs.gll._

trait Example[A] extends Parsers {
  
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("=============================")
      
      val results = parser(LineStream(Source fromFile file))
      
      if (results exists { _.isInstanceOf[Success[A]] }) {
        handleSuccesses(for (Success(tree, _) <- results) yield tree)
      } else {
        val sorted = results sort { _.tail.length < _.tail.length }
        val length = sorted.head.tail.length
        
        for (Failure(msg, tail) <- sorted takeWhile { _.tail.length == length }) {
          val pattern = "  error:%%d: %s%n    %%s%n    %%s%n".format(msg)
          tail.printError(pattern)(System.err)
        }
      }
      
      println()
    }
  }
  
  def parser: Parser[A]
  
  def handleSuccesses(forest: List[A])
}
