import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object TerminalSpecs extends Specification with ScalaCheck with Parsers {
  import Prop._
  import StreamUtils._
  
  "terminal parser" should {
    "parse single tokens" in {
      val p = literal("test")
      
      p("test") must beLike {
        case Success("test", LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "produce 'expected' failure message" in {
      val p = literal("foo")
      
      p("bar") must beLike {
        case Failure(ExpectedLiteral("foo", "bar"), LineStream('b', 'a', 'r')) #:: SNil => true
        case _ => false
      }
      
      p("test") must beLike {
        case Failure(ExpectedLiteral("foo", "tes"), LineStream('t', 'e', 's', 't')) #:: SNil => true
        case _ => false
      }
    }
    
    "canonicalize failure message" in {
      val p = literal("")
      
      p("\n") must beLike {
        case Failure(UnexpectedTrailingChars("\\n"), LineStream('\n')) #:: SNil => true
        case _ => false
      }
      
      val p2 = literal("a")
      
      p2("\n") must beLike {
        case Failure(ExpectedLiteral("a", "\\n"), LineStream('\n')) #:: SNil => true
        case _ => false
      }
      
      p2("\r") must beLike {
        case Failure(ExpectedLiteral("a", "\\r"), LineStream('\r')) #:: SNil => true
        case _ => false
      }
      
      p2("\t") must beLike {
        case Failure(ExpectedLiteral("a", "\\t"), LineStream('\t')) #:: SNil => true
        case _ => false
      }
      
      p2("\f") must beLike {
        case Failure(ExpectedLiteral("a", "\\f"), LineStream('\f')) #:: SNil => true
        case _ => false
      }
    }
    
    "detect an unexpected end of stream" in {
      val p = literal("foo")
      
      p(LineStream('f')) must beLike {
        case Failure(UnexpectedEndOfStream(Some("foo")), LineStream('f')) #:: SNil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(Some("foo")), LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "parse the empty string" in {
      val p = literal("")
      
      p(LineStream()) must beLike {
        case Success("", LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      import edu.uwm.cs.util.UniversalCharSet
      
      val prop = forAll { s: String =>
        if (s.length == 0)
          literal(s).first == UniversalCharSet     // TODO file bug report for non-working specs matchers
        else
          literal(s).first == Set(s charAt 0)
      }
      
      prop must pass
    }
    
    "map results according to a function" in {
      val p = "test" ^^ { _.length }
      
      p("test") must beLike {
        case Success(4, LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "map results according to a value" in {
      val p = "test" ^^^ 42
      
      p("test") must beLike {
        case Success(42, LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "map results with stream tail" in {
      var in1: LineStream = null
      val p1 = "foo" ^# { (in, s) =>
        in1 = in
        s
      }
      
      var in2: LineStream = null
      val p2 = "bar" ^# { (in, s) =>
        in2 = in
        s
      }
      
      val p = p1 ~ "\n" ~> p2
      
      p("foo\nbar") must beLike {
        case Success("bar", LineStream()) #:: SNil => true
        case _ => false
      }
      
      in1.line mustEqual "foo"
      in1.lineNum mustEqual 1
      in1.head mustBe 'f'
      in1.toString mustEqual "foo\nbar"
      
      in2.line mustEqual "bar"
      in2.lineNum mustEqual 2
      in2.head mustBe 'b'
      in2.toString mustEqual "bar"
    }
  }
  
  "terminal sequential parser" should {
    "parse multiple tokens" in {
      val p = "te" ~ "st"
      
      p("test") must beLike {
        case Success("te" ~ "st", LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "produce 'expected' error message" in {
      val p = "te" ~ "st"
      
      p("foo") must beLike {
        case Failure(ExpectedLiteral("te", "fo"), LineStream('f', 'o', 'o')) #:: SNil => true
        case _ => false
      }
      
      p("tefoo") must beLike {
        case Failure(ExpectedLiteral("st", "fo"), LineStream('f', 'o', 'o')) #:: SNil => true
        case _ => false
      }
    }
    
    "detect an unexpected end of stream" in {
      val p = "te" ~ "st"
      
      p(LineStream('t')) must beLike {
        case Failure(UnexpectedEndOfStream(Some("te")), LineStream('t')) #:: SNil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(Some("te")), LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("tes") must beLike {
        case Failure(UnexpectedEndOfStream(Some("st")), LineStream('s')) #:: SNil => true
        case _ => false
      }
      
      p("te") must beLike {
        case Failure(UnexpectedEndOfStream(Some("st")), LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      import edu.uwm.cs.util.UniversalCharSet
      
      val prop = forAll { strs: List[String] =>
        (strs.length > 0 && (strs exists { _.length > 0 })) ==> {
          val p = strs.map(literal).reduceLeft[Parser[Any]] { _ ~ _ }
          
          val composite = strs.mkString
          val first = if (composite.length == 0) UniversalCharSet else Set(composite charAt 0)
          
          if (p.first.size == 0 && first.size == 0)
            true
          else
            p.first == first    // TODO file bug report
        }
      }
      
      prop must pass
    }
  }
}
