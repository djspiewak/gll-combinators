import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object CompoundSpecs extends Specification with ImplicitConversions with ScalaCheck {
  import Prop._
  import StreamUtils._
  
  "compound non-terminal parsers" should {
    "parse an unambiguous right-recursive grammar" in {
      def p: Parser[String] = (
          "a" ~ p ^^ { case a ~ b => a + b }
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data toProperStream) must beLike {
          case Success(`data`, Stream()) :: Nil => true
          case _ => false
        }
      }

      p mustNot throwA[Throwable]
      
      check("a")
      check("aa")
      check("aaaaaaaa")
      check("aaaaa")
    }

    "parse an LL(1) right-recursive grammar" in {
      def p: Parser[String] = (
          "a" ~ p ^^ { case a ~ b => a + b }
        | "b"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data toProperStream) must beLike {
          case Success(`data`, Stream()) :: Nil => true
          case _ => false
        }
      }

      p mustNot throwA[Throwable]
      
      check("b")
      check("ab")
      check("aaaaaaab")
      check("aaaab")
    }
    
    "parse an unambiguous left-recursive grammar" in {
      def p: Parser[String] = (
          p ~ "a" ^^ { case a ~ b => a + b }
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data toProperStream) must beLike {
          case Success(`data`, Stream()) :: Nil => true
          case _ => false
        }
      }
      
      p mustNot throwA[Throwable]
      
      check("a")
      check("aa")
      check("aaaaaaaa")
      check("aaaaa")
    }
  }
}
