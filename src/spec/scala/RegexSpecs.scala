import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object RegexSpecs extends Specification with ScalaCheck with RegexParsers {
  import Prop._
  
  "regex parsers" should {
    "match, consume and return from a regex match" in {
      {
        val p: Parser[String] = """(\d{1,3}\.){3}\d{1,3}"""r
        
        p("192.168.101.2") must beLike {
          case Success("192.168.101.2", Stream()) :: Nil => true
          case _ => false
        }
      }
      
      {
        val p: Parser[String] = """\d+"""r
        
        var passed = false
        p.queue(null, "1234daniel".foldRight(Stream[Char]()) { Stream.cons(_, _) }) {
          case Success(res, tail) => {
            if (passed) {
              fail("Produced more than one result")
            } else {
              passed = true
              
              res mustEqual "1234"
              tail zip Stream('d', 'a', 'n', 'i', 'e', 'l') forall { case (a, b) => a == b } mustBe true
            }
          }
          
          case _ =>
        }
        
        passed mustBe true
      }
    }
    
    "produce 'expected' error message" in {
      val p: Parser[String] = """(\d{1,3}\.){3}\d{1,3}"""r
      
      {
        val data = "123.457.321".foldRight(Stream[Char]()) { Stream.cons(_, _) }
        
        p(data) must beLike {
          case Failure("""Expected /(\d{1,3}\.){3}\d{1,3}/""", `data`) :: Nil => true
          case _ => false
        }
      }
      
      {
        val data = "123.457.321.sdf".foldRight(Stream[Char]()) { Stream.cons(_, _) }
        
        p(data) must beLike {
          case Failure("""Expected /(\d{1,3}\.){3}\d{1,3}/""", `data`) :: Nil => true
          case _ => false
        }
      }
      
      {
        p(Stream()) must beLike {
          case Failure("""Expected /(\d{1,3}\.){3}\d{1,3}/""", Stream()) :: Nil => true
          case _ => false
        }
      }
    }
    
    "eat leading whitespace" in {
      val p = literal("daniel")
      
      p("daniel") must beLike {
        case Success("daniel", Stream()) :: Nil => true
        case _ => false
      }
      
      p("  daniel") must beLike {
        case Success("daniel", Stream()) :: Nil => true
        case _ => false
      }
      
      p("\tdaniel") must beLike {
        case Success("daniel", Stream()) :: Nil => true
        case _ => false
      }
      
      p("""
      daniel""") must beLike {
        case Success("daniel", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "eat trailing whitespace" in {
      val p = literal("daniel")
      
      p("daniel    ") must beLike {
        case Success("daniel", Stream()) :: Nil => true
        case _ => false
      }
      
      p("daniel\t") must beLike {
        case Success("daniel", Stream()) :: Nil => true
        case _ => false
      }
      
      p("daniel\n") must beLike {
        case Success("daniel", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "have universal FIRST set" in {
      """\d""".r.first mustBe edu.uwm.cs.UniversalCharSet
    }
  }
}
