import edu.uwm.cs.gll._
import edu.uwm.cs.util._

import org.specs._
import org.scalacheck._

object CompoundSpecs extends Specification with ImplicitConversions with ScalaCheck {
  import Prop._
  
  "compound non-terminal parsers" should {
    "parse an unambiguous right-recursive grammar" in {
      def p: Parser[String] = (
          "a" ~ p ^^ { _ + _ }
        | "a"
      )
   
      // assumes data =~ /a+/
      def check(data: String) {
        p(data) must beLike {
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
        p(data) must beLike {
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
      lazy val p: Parser[String] = (
          p ~ "a" ^^ { _ + _ }
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data) must beLike {
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
      val b = "" | "a"
      
      lazy val c: Parser[String] = (
          "b"
        | b ~ c ~ "b" ^^ { _ + _ + _ }
        | "b" ~ "b"   ^^^ "bb"
      )
      
      val s = (
          c ~ "a" ^^ { _ + _ }
        | "d"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        s(data) must beLike {
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
      lazy val s: Parser[String] = (
          "b"
        | s ~ s     ^^ { _ + _ }
        | s ~ s ~ s ^^ { _ + _ + _ }
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        s(data) must beLike {
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
      lazy val s: Parser[String] = (
          "b"
        | s ~ s ~ (s | "") ^^ { _ + _ + _ }
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        s(data) must beLike {
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
    
    "parse an unambiguous arithmetic grammar" in {
      import StreamUtils._
      
      object MathParser extends RegexParsers {
        lazy val expr: Parser[Int] = (
            expr ~ "*" ~ factor   ^^ { (e1, _, e2) => e1 * e2 }
          | expr ~ "/" ~ factor   ^^ { (e1, _, e2) => e1 / e2 }
          | factor
        )
        
        lazy val factor: Parser[Int] = (
            factor ~ "+" ~ term   ^^ { (e1, _, e2) => e1 + e2 }
          | factor ~ "-" ~ term   ^^ { (e1, _, e2) => e1 - e2 }
          | term
        )
        
        lazy val term: Parser[Int] = (
            "(" ~> expr <~ ")"
          | "-" ~> term           ^^ { -_ }
          | """\d+""".r           ^^ { _.toInt }
        )
      }
      
      val input = """1 + 6 / 3 * 2
- -2 + 3 /

(1 + 2)"""
      
      MathParser.expr(input toProperStream) must beLike {
        case Success(5, Stream()) :: Nil => true
        case _ => false
      }
    }
  }
  
  "repeated non-terminal parsers" should {
    "repeat 0..n times" in {
      val p = literal("123")*
      
      p("123") must beLike {
        case Success(List("123"), Stream()) :: Nil => true
        case _ => false
      }
      
      p("123123123123123123123123123123123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), Stream()) :: Nil => true
        case _ => false
      }
      
      p(Stream()) must beLike {
        case Success(Nil, Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "repeat 1..n times" in {
      val p = literal("123")+
      
      p("123") must beLike {
        case Success(List("123"), Stream()) :: Nil => true
        case _ => false
      }
      
      p("123123123123123123123123123123123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "repeat 0..1 times" in {
      val p = literal("123")?
      
      p("123") must beLike {
        case Success(Some("123"), Stream()) :: Nil => true
        case _ => false
      }
      
      p(Stream()) must beLike {
        case Success(None, Stream()) :: Nil => true
        case _ => false
      }
    }
  }
  
  "monadic parsers" should {
    "compose using bind" in {
      val p = literal("a") flatMap { _ => literal("b") }
      
      p(Stream('a', 'b')) must beLike {
        case Success("b", Stream()) :: Nil => true
        case _ => false
      }
      
      p(Stream('a', 'c')) must beLike {
        case Failure("Expected 'b' got 'c'", Stream('c')) :: Nil => true
        case _ => false
      }
      
      p.first mustEqual Set('a')
    }
    
    "compose using map" in {
      val p = literal("1") map { _.toInt }
      
      p(Stream('1')) must beLike {
        case Success(1, Stream()) :: Nil => true
        case _ => false
      }
      
      p(Stream('2')) must beLike {
        case Failure("Expected '1' got '2'", Stream('2')) :: Nil => true
        case _ => false
      }
      
      p.first mustEqual Set('1')
    }
    
    "compose using orElse" in {
      val p = literal("a") orElse literal("b")
      
      p(Stream('a')) must beLike {
        case Success("a", Stream()) :: Nil => true
        case _ => false
      }
      
      p(Stream('b')) must beLike {
        case Success("b", Stream()) :: Nil => true
        case _ => false
      }
      
      p(Stream('c')) must beLike {
        case Failure("Unexpected value in stream: 'c'", Stream('c')) :: Nil => true
        case _ => false
      }
      
      p.first mustEqual Set('a', 'b')
    }
    
    "filter" in {
      val p = ("a" | "b") filter { _ == "a" }
      
      p(Stream('a')) must beLike {
        case Success("a", Stream()) :: Nil => true
        case _ => false
      }
      
      p(Stream('b')) must beLike {
        case Failure("Syntax error", Stream('b')) :: Nil => true
        case _ => false
      }
      
      p(Stream('c')) must beLike {
        case Failure("Unexpected value in stream: 'c'", Stream('c')) :: Nil => true
        case _ => false
      }
      
      p.first mustEqual Set('a', 'b')
    }
  }
}
