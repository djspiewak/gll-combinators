import edu.uwm.cs.gll._
import org.specs._

object ParserSpecs extends Specification with ImplicitConversions {
  "terminal parsers" should {
    "parse single tokens" in {
      val p = literal("test")
      
      p("test" toStream) must beLike {
        case Success("test", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "parse multiple tokens" in {
      val p = "te" ~ "st"
      
      p("test" toStream) must beLike {
        case Success("te" ~ "st", Stream()) :: Nil => true
        case _ => false
      }
    }
  }
}
