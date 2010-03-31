import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object RegexSpecs extends Specification with ScalaCheck with RegexParsers {
  import Prop._
  import StreamUtils._
  
  "regex parsers" should {
    "match, consume and return from a regex match" in {
      {
        val p: Parser[String] = """(\d{1,3}\.){3}\d{1,3}"""r
        
        p("192.168.101.2") must beLike {
          case Success("192.168.101.2", LineStream()) #:: SNil => true
          case _ => false
        }
      }
      
      {
        val p: Parser[String] = """\d+"""r
        
        p("1234daniel") must beLike {
          case Failure("Unexpected trailing characters: 'daniel'", LineStream('d', 'a', 'n', 'i', 'e', 'l')) #:: SNil => true
          case _ => false
        }
      }
    }
    
    "gracefully error on null regexp" in {
      regex(null) must throwA(new NullPointerException("Cannot parse a null regular expression"))
    }
    
    "produce 'expected' error message" in {
      val p: Parser[String] = """(\d{1,3}\.){3}\d{1,3}"""r
      
      {
        val data = LineStream("123.457.321")
        
        p(data) must beLike {
          case Failure("""Expected /(\d{1,3}\.){3}\d{1,3}/""", `data`) #:: SNil => true
          case _ => false
        }
      }
      
      {
        val data = LineStream("123.457.321.sdf")
        
        p(data) must beLike {
          case Failure("""Expected /(\d{1,3}\.){3}\d{1,3}/""", `data`) #:: SNil => true
          case _ => false
        }
      }
      
      {
        p(LineStream()) must beLike {
          case Failure("""Expected /(\d{1,3}\.){3}\d{1,3}/""", LineStream()) #:: SNil => true
          case _ => false
        }
      }
    }
    
    "eat leading whitespace" in {
      val p = literal("daniel")
      
      p("daniel") must beLike {
        case Success("daniel", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("  daniel") must beLike {
        case Success("daniel", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("\tdaniel") must beLike {
        case Success("daniel", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("""
      daniel""") must beLike {
        case Success("daniel", LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "eat trailing whitespace" in {
      val p = literal("daniel")
      
      p("daniel    ") must beLike {
        case Success("daniel", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("daniel\t") must beLike {
        case Success("daniel", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p("daniel\n") must beLike {
        case Success("daniel", LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "have universal FIRST set" in {
      """\d""".r.first mustBe edu.uwm.cs.util.UniversalCharSet
    }
    
    "negate using a regexp parser" in {
      val p1 = "test" \ ("test" | "ing")
      
      p1("test") must beLike {
        case Failure("Expected 'test' and not /(test)|(ing)/ in 'test'", LineStream(tail @ _*)) #:: SNil =>
          tail.mkString mustEqual "test"
        
        case _ => false
      }
      
      val p2 = "test" \ ("blah" | "ing ")
      
      p2("test") must beLike {
        case Success("test", LineStream()) #:: SNil => true
        case _ => false
      }
      
      p2("ing ") must beLike {
        case Failure("Expected 'test' got 'ing '", LineStream(tail @ _*)) #:: SNil =>
          tail.mkString mustEqual "ing "
        
        case _ => false
      }
      
      p2("blah") must beLike {
        case Failure("Expected 'test' got 'blah'", LineStream(tail @ _*)) #:: SNil =>
          tail.mkString mustEqual "blah"
        
        case _ => false
      }
    }
  }
}
