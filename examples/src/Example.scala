package common

import scala.io.Source
import com.codecommit.gll._

trait Example[A] extends Parsers {
  
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("=============================")
      
      val results = parser(LineStream(Source fromFile file))
      
      if (results exists { _.isInstanceOf[Success[A]] }) {
        handleSuccesses(for (Success(tree, _) <- results) yield tree)
      } else {
        val sorted = results.toList sortWith { _.tail.length < _.tail.length }
        val length = sorted.head.tail.length
        
        for (Failure(msg, tail) <- sorted takeWhile { _.tail.length == length }) {
          val pattern = "  error:%%d: %s%n    %%s%n    %%s%n".format(msg)
          tail.printError(pattern)(System.err)
        }
      }
      
      println()
    }
  }
  
  def test(file: String): Boolean = {
    val src = Source.fromInputStream(getClass().getResourceAsStream(file))
    val results: Stream[Result[A]] = parser(LineStream(src))
    if (results exists { _.isInstanceOf[Success[A]] }) {
      // suppress stdout:
      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        handleSuccesses(for (Success(tree, _) <- results) yield tree)
      }
      return true
    } else {
      return false
    }
  }

  def parser: Parser[A]
  
  def handleSuccesses(forest: Stream[A])
}
