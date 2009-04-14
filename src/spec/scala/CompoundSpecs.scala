import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object CompoundSpecs extends Specification with ImplicitConversions with ScalaCheck {
  import Prop._
  
  "compound non-terminal parsers" should {
    "parse an unambiguous right-recursive grammar" in {
      def p: Parser[String] = (
          "a" ~ p ^^ { case a ~ b => a + b }
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data toStream) must beLike {
          case Success(`data`, Stream()) :: Nil => true   // TODO
          case _ => false
        }
      }
      
      p mustNot throwA[Throwable]
      
      check("a")
      check("aaaaaaaa")
      check("aaaaa")
      check("aa")
    }
    
    "parse an unambiguous left-recursive grammar" in {
      def p: Parser[String] = (
          p ~ "a" ^^ { case a ~ b => a + b }
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data toStream) must beLike {
          case Success(`data`, Stream()) => true
          case _ => false
        }
      }
      
      check("a")
      check("aaaaaaaa")
      check("aaaaa")
      check("aa")
    }
  }
}
