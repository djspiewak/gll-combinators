import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object TerminalSpecs extends Specification with ScalaCheck with ImplicitConversions {
  import Prop._
  import StreamUtils._
  
  "terminal parser" should {
    "parse single tokens" in {
      val p = literal("test")
      
      p("test" toProperStream) must beLike {
        case Success("test", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "parse the empty string" in {
      val p = literal("")
      
      p("test" toProperStream) must beLike {
        case Success("", Stream('t', 'e', 's', 't')) :: Nil => true     // should I keep this??
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      val prop = forAll { s: String =>
        if (s.length == 0)
          literal(s).first == Set()
        else
          literal(s).first == Set(s charAt 0)
      }
      
      prop must pass
    }
    
    "map results according to a function" in {
      val p = "test" ^^ { _.length }
      
      p("test" toProperStream) match {
        case Success(4, Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "map results according to a value" in {
      val p = "test" ^^^ 42
      
      p("test" toProperStream) match {
        case Success(42, Stream()) :: Nil => true
        case _ => false
      }
    }
  }
  
  "terminal sequential parser" should {
    "parse multiple tokens" in {
      val p = "te" ~ "st"
      
      p("test" toProperStream) must beLike {
        case Success("te" ~ "st", Stream()) :: Nil => true
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
