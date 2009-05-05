import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object TerminalSpecs extends Specification with ScalaCheck with ImplicitConversions {
  import Prop._
  
  "terminal parser" should {
    "parse single tokens" in {
      val p = literal("test")
      
      p("test") must beLike {
        case Success("test", LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "produce 'expected' failure message" in {
      val p = literal("foo")
      
      p("bar") must beLike {
        case Failure("Expected 'foo' got 'bar'", LineStream('b', 'a', 'r')) :: Nil => true
        case _ => false
      }
      
      p("test") must beLike {
        case Failure("Expected 'foo' got 'tes'", LineStream('t', 'e', 's', 't')) :: Nil => true
        case _ => false
      }
    }
    
    "detect an unexpected end of stream" in {
      val p = literal("foo")
      
      p(LineStream('f')) must beLike {
        case Failure("Unexpected end of stream (expected 'foo')", LineStream('f')) :: Nil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure("Unexpected end of stream (expected 'foo')", LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "parse the empty string" in {
      val p = literal("")
      
      p(LineStream()) must beLike {
        case Success("", LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      val prop = forAll { s: String =>
        if (s.length == 0)
          literal(s).first mustEqual Set()
        else
          literal(s).first mustEqual Set(s charAt 0)
      }
      
      prop must pass
    }
    
    "map results according to a function" in {
      val p = "test" ^^ { _.length }
      
      p("test") match {
        case Success(4, LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "map results according to a value" in {
      val p = "test" ^^^ 42
      
      p("test") match {
        case Success(42, LineStream()) :: Nil => true
        case _ => false
      }
    }
  }
  
  "terminal sequential parser" should {
    "parse multiple tokens" in {
      val p = "te" ~ "st"
      
      p("test") must beLike {
        case Success("te" ~ "st", LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "produce 'expected' error message" in {
      val p = "te" ~ "st"
      
      p("foo") must beLike {
        case Failure("Expected 'te' got 'fo'", LineStream('f', 'o', 'o')) :: Nil => true
        case _ => false
      }
      
      p("tefoo") must beLike {
        case Failure("Expected 'st' got 'fo'", LineStream('f', 'o', 'o')) :: Nil => true
        case _ => false
      }
    }
    
    "detect an unexpected end of stream" in {
      val p = "te" ~ "st"
      
      p(LineStream('t')) must beLike {
        case Failure("Unexpected end of stream (expected 'te')", LineStream('t')) :: Nil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure("Unexpected end of stream (expected 'te')", LineStream()) :: Nil => true
        case _ => false
      }
      
      p("tes") must beLike {
        case Failure("Unexpected end of stream (expected 'st')", LineStream('s')) :: Nil => true
        case _ => false
      }
      
      p("te") must beLike {
        case Failure("Unexpected end of stream (expected 'st')", LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      val prop = forAll { strs: List[String] =>
        strs.length > 0 ==> {
          val p = strs.map(literal).reduceLeft[Parser[Any]] { _ ~ _ }
          
          val composite = strs.mkString
          val first = if (composite.length == 0) Set() else Set(composite charAt 0)
          
          if (p.first.size == 0 && first.size == 0)
            p.first.size mustBe first.size      // tautology
          else
            p.first must haveTheSameElementsAs(first)
        }
      }
      
      prop must pass
    }
  }
}
