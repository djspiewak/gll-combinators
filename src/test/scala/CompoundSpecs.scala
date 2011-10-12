import edu.uwm.cs.gll._
import edu.uwm.cs.util._

import org.specs._
import org.scalacheck._

object CompoundSpecs extends Specification with Parsers with ScalaCheck {
  import Prop._
  import StreamUtils._
  
  "compound non-terminal parsers" should {
    "parse an unambiguous right-recursive grammar" in {
      lazy val p: Parser[String] = (
          "a" ~ p ^^ { _ + _ }
        | "a"
      )
   
      // assumes data =~ /a+/
      def check(data: String) {
        p(data) must beLike {
          case Success(`data`, LineStream()) #:: SNil => true
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
      lazy val p: Parser[String] = (
          "a" ~ p ^^ { _ + _ }
        | "b"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data) must beLike {
          case Success(`data`, LineStream()) #:: SNil => true
          case _ => false
        }
      }

      p mustNot throwA[Throwable]
      
      check("b")
      check("ab")
      check((0 to 256).foldLeft("") { (s, _) => s + "a" } + "b")
      check("aaaab")
    }
    
    "parse an unambiguous left-recursive grammar (recursive-major)" in {
      lazy val p: Parser[String] = (
          p ~ "a" ^^ { _ + _ }
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data) must beLike {
          case Success(`data`, LineStream()) #:: SNil => true
          case _ => false
        }
      }
      
      p mustNot throwA[Throwable]
      
      check("a")
      check("aa")
      check((0 to 256).foldLeft("") { (s, _) => s + "a" })
      check("aaaaa")
    }
    
    "parse an unambiguous left-recursive grammar (recursive-minor)" in {
      lazy val p: Parser[String] = (
          "a"
        | p ~ "a" ^^ { _ + _ }
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data) must beLike {
          case Success(`data`, LineStream()) #:: SNil => true
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
        | "b" ~ "b"  ^^^ "bb"
      )
      
      val s = (
          c ~ "a" ^^ { _ + _ }
        | "d"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        s(data) must beLike {
          case Success(`data`, LineStream()) #:: _ => true     // we don't care how many derivations, just that it works
          case _ => false
        }
      }
      
      s mustNot throwA[Throwable]
      
      check("abba")
      check("bbbbbbba")
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
          case Success(`data`, LineStream()) #:: _ => true     // we don't care how many derivations, just that it works
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
          case Success(`data`, LineStream()) #:: _ => true     // we don't care how many derivations, just that it works
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
      object MathParser extends RegexParsers {
        lazy val expr: Parser[Int] = (
            expr ~ "+" ~ term     ^^ { (e1, _, e2) => e1 + e2 }
          | expr ~ "-" ~ term     ^^ { (e1, _, e2) => e1 - e2 }
          | term
        )
        
        lazy val term: Parser[Int] = (
            term ~ "*" ~ factor   ^^ { (e1, _, e2) => e1 * e2 }
          | term ~ "/" ~ factor   ^^ { (e1, _, e2) => e1 / e2 }
          | factor
        )
        
        lazy val factor: Parser[Int] = (
            "(" ~> expr <~ ")"
          | "-" ~> factor         ^^ { -_ }
          | """\d+""".r           ^^ { _.toInt }
        )
      }
      
      val input = """1 + 6 / 3 * 2
                     - -2 + 3 /
                     
                     (1 + 2)"""
      
      MathParser.expr(input) must beLike {
        case Success(8, LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "produce all possible results with recursive ambiguity" in {
      object ConfigParser extends RegexParsers {
        override val whitespace = """[ \t]+"""r  // process newlines separately
        
        lazy val parser = """\s*""".r ~> config <~ """\s*""".r     // allow leading/trailing whitespace
        
        private lazy val config = (pairs <~ newline).? ~ sections ^^ { _.getOrElse(Map()) ++ _ }
        
        private lazy val sections: Parser[Map[String, String]] = (
            sections ~ newline ~ section  ^^ { (map1, _, map2) => map1 ++ map2 }
          | section
          | ""                            ^^^ Map()
        )
        
        private lazy val section = ("[" ~> id <~ "]") ~ newline ~ config ^^ { (id, _, map) =>
          val back = for ((key, value) <- map) yield (id + '.' + key) -> value
          back.foldLeft(Map[String, String]()) { _ + _ }
        }
        
        private lazy val pairs: Parser[Map[String, String]] = (
            pairs ~ newline ~ pair    ^^ { (map, _, tuple) => map + tuple }
          | pair                      ^^ { Map(_) }
        )
        
        private lazy val pair = id ~ "=" ~ data    ^^ { (key, _, value) => key -> value }
        
        private val id = """\w+"""r
        private val data = """[^\s]+"""r
        private val newline = """(\n\r|\r\n|\n|\r)"""r
      }
      
      val input = """
[core]
    version = 0
    filemode = true

[remote]
    url = hero
    fetch = boo
"""
      
      ConfigParser.parser(input) must beLike {
        case Success(map1, _) #:: Success(map2, _) #:: SNil => {
          if (map1 contains "core.remote.url") {
            map1 must haveKey("core.filemode")
            map1 must haveKey("core.remote.fetch")
            map1 mustNot haveKey("remote.url")
            
            map2 must haveKey("remote.url")
            map2 must haveKey("core.filemode")
            map2 mustNot haveKey("core.remote.fetch")
          } else {
            map1 must haveKey("remote.url")
            map1 must haveKey("core.filemode")
            map1 mustNot haveKey("core.remote.fetch")
            
            map2 must haveKey("core.filemode")
            map2 must haveKey("core.remote.fetch")
            map2 mustNot haveKey("remote.url")
          }
          
          true
        }
        
        case _ => false
      }
    }
    
    "avoid greedy matching on a local ambiguity" in {
      sealed trait Expr
      
      case class Binding(formals: Vector[String]) extends Expr
      case class TicVar(id: String) extends Expr
      case class Dispatch(actuals: Vector[Expr]) extends Expr
        
      object TestParser extends RegexParsers {
        lazy val expr: Parser[Expr] = (
            "(" ~ formals ~ ")" ~ ":=" ^^ { (_, fs, _, _) => Binding(fs) }
          | "(" ~ expr ~ ")" ^^ { (_, as, _) => Dispatch(Vector(as)) }
          | ticId ^^ TicVar
        )
  
        lazy val formals: Parser[Vector[String]] = (
            formals ~ "," ~ ticId ^^ { (fs, _, f) => fs :+ f }
          | ticId                 ^^ { Vector(_) }
        )
        
        val ticId = "a"
      }
      
      TestParser.expr("(a) :=") mustNot throwA[ClassCastException]
    }
    
    "compute FIRST for nested left-recursion" in {
      object ComplexParser extends RegexParsers {
        lazy val exp: Parser[Any] = (
            n
          | "(" ~ commaExps ~ ")"
          | exp ~ exp
        ) ^^^ null
        
        lazy val commaExps: Parser[Any] = (
            exp
          | commaExps ~ "," ~ exp
        )
        
        val n = """\d+"""r
      }
      
      ComplexParser.exp.first must containAll(Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ' ', '\t', '\n', '\r', '('))
    }
    
    "handle nested left-recursion" in {
      object ComplexParser extends RegexParsers {
        lazy val exp: Parser[Any] = (
            n
          | "(" ~ commaExps ~ ")"
          | exp ~ exp
        ) ^^^ null
        
        lazy val commaExps: Parser[Any] = (
            exp
          | commaExps ~ "," ~ exp
        )
        
        val n = """\d+"""r
      }
      
      import ComplexParser._
      
      exp("(0,0) 2") must beLike {
        case Success(_, LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "negate using a terminal parser" in {
      val p1 = "test" \ "test"
      
      p1("test") must beLike {
        case Failure(SyntaxError, LineStream(tail @ _*)) #:: SNil => 
          tail.mkString mustEqual "test"
        
        case _ => false
      }
      
      val p2 = "test" \ "ing"
      
      p2("test") must beLike {
        case Success("test", LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "negate within a sequence" in {
      import RegexParsers._
      
      val p = ("a|b".r \ "a") ~ "c" ^^ { _ + _ }
      
      p("bc") must beLike {
        case Success("bc", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("ac") must beLike {
        case Failure(SyntaxError, _) #:: _ => true
        case _ => false
      }
    }
  }
  
  "repeated non-terminal parsers" should {
    "repeat 0..n times" in {
      val p = literal("123")*
      
      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("123123123123123123123123123123123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Success(Nil, LineStream()) #:: SNil => true
        case _ => false
      }
    }
    "repeat 0..n times with separator" in {
      val p = literal("123") * ","
      
      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("123,123,123,123,123,123,123,123,123,123,123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Success(Nil, LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "repeat 1..n times" in {
      val p = literal("123")+
      
      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("123123123123123123123123123123123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "repeat 1..n times with separator" in {
      val p = literal("123") + ","
      
      p("123") must beLike {
        case Success(List("123"), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("123,123,123,123,123,123,123,123,123,123,123") must beLike {
        case Success(List("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123"), LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "repeat 0..1 times" in {
      val p = literal("123")?
      
      p("123") must beLike {
        case Success(Some("123"), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Success(None, LineStream()) #:: SNil => true
        case _ => false
      }
    }
  }
  
  "monadic parsers" should {
    "compose using bind" in {
      val p = literal("a") flatMap { _ => literal("b") }
      
      p(LineStream('a', 'b')) must beLike {
        case Success("b", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream('a', 'c')) must beLike {
        case Failure(ExpectedLiteral("b", "c"), LineStream('c')) #:: SNil => true
        case _ => false
      }
      
      p.first mustEqual Set('a')
    }
    
    "compose using map" in {
      val p = literal("1") map { _.toInt }
      
      p(LineStream('1')) must beLike {
        case Success(1, LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream('2')) must beLike {
        case Failure(ExpectedLiteral("1", "2"), LineStream('2')) #:: SNil => true
        case _ => false
      }
      
      p.first mustEqual Set('1')
    }
    
    "compose using orElse" in {
      val p = literal("a") orElse literal("b")
      
      p(LineStream('a')) must beLike {
        case Success("a", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream('b')) must beLike {
        case Success("b", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream('c')) must beLike {
        case Failure(UnexpectedChars("c"), LineStream('c')) #:: SNil => true
        case _ => false
      }
      
      p.first mustEqual Set('a', 'b')
    }
    
    "filter" in {
      val p = ("a" | "b") filter { _ == "a" }
      
      p(LineStream('a')) must beLike {
        case Success("a", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p(LineStream('b')) must beLike {
        case Failure(SyntaxError, LineStream('b')) #:: SNil => true
        case _ => false
      }
      
      p(LineStream('c')) must beLike {
        case Failure(UnexpectedChars("c"), LineStream('c')) #:: SNil => true
        case _ => false
      }
      
      p.first mustEqual Set('a', 'b')
    }
  }
}
