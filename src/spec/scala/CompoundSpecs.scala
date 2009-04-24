import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object CompoundSpecs extends Specification with ImplicitConversions with ScalaCheck {
  import Prop._
  import StreamUtils._
  
  "compound non-terminal parsers" should {
    "parse an unambiguous right-recursive grammar" in {
      def p: Parser[String] = (
          "a" ~ p ^^ { _ + _ }
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
      check((0 to 256).foldLeft("") { (s, _) => s + "a" })
      check("aaaaa")
    }

    "parse an LL(1) right-recursive grammar" in {
      def p: Parser[String] = (
          "a" ~ p ^^ { _ + _ }
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
      check((0 to 256).foldLeft("") { (s, _) => s + "a" } + "b")
      check("aaaab")
    }
    
    "parse an unambiguous left-recursive grammar" in {
      def p: Parser[String] = (
          p ~ "a" ^^ { _ + _ }
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
      check((0 to 256).foldLeft("") { (s, _) => s + "a" })
      check("aaaaa")
    }
    
    "parse Gamma_1" in {
      def b = "" | "a"
      
      def c: Parser[String] = (
          "b"
        | b ~ c ~ "b" ^^ { _ + _ + _ }
        | "b" ~ "b"   ^^^ "bb"
      )
      
      def s = (
          c ~ "a" ^^ { _ + _ }
        | "d"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        s(data toProperStream) must beLike {
          case Success(`data`, Stream()) :: _ => true     // we don't care how many derivations, just that it works
          case _ => false
        }
      }
      
      s mustNot throwA[Throwable]
      
      check("abba")
      check("d")
      check("aaaaaaaabbbbbbbbba")
      
      val prefix = (0 to 20).foldLeft("") { (str, _) => str + "a" }
      val suffix = (0 to 50).foldLeft("") { (str, _) => str + "b" }
      check(prefix + suffix + "a")
      
      check("bba")
    }
    
    "parse Gamma_2" in {
      def s: Parser[String] = (
          "b"
        | s ~ s     ^^ { _ + _ }
        | s ~ s ~ s ^^ { _ + _ + _ }
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        s(data toProperStream) must beLike {
          case Success(`data`, Stream()) :: _ => true     // we don't care how many derivations, just that it works
          case _ => false
        }
      }
      
      s mustNot throwA[Throwable]
      
      check("b")
      check("bb")
      check((0 to 50).foldLeft("") { (str, _) => str + "b" })
      check("bbbbb")
    }
    
    "parse Gamma_2*" in {
      def s: Parser[String] = (
          "b"
        | s ~ s ~ (s | "") ^^ { _ + _ + _ }
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        s(data toProperStream) must beLike {
          case Success(`data`, Stream()) :: _ => true     // we don't care how many derivations, just that it works
          case _ => false
        }
      }
      
      s mustNot throwA[Throwable]
      
      check("b")
      check("bb")
      check((0 to 50).foldLeft("") { (str, _) => str + "b" })
      check("bbbbb")
    }
  }
}
