import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object ParserSpecs extends Specification with ScalaCheck with ImplicitConversions {
  import Prop._
  
  "literal parser" should {
    "parse single tokens" in {
      val p = literal("test")
      
      p("test" toStream) must beLike {
        case Success("test", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "parse the empty string" in {
      val p = literal("")
      
      p("test" toStream) must beLike {
        case Success("", Stream('t', 'e', 's', 't')) :: Nil => true
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
  }
  
  "terminal sequential parser" should {
    "parse multiple tokens" in {
      val p = "te" ~ "st"
      
      p("test" toStream) must beLike {
        case Success("te" ~ "st", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      val prop = forAll { strs: List[String] =>
        strs.length > 0 ==> {
          val p = strs.map(literal).reduceLeft[Parser[Any]] { _ ~ _ }
          val first = strs.foldLeft(Set[Char]()) { (set, str) =>
            if (set.size == 0 && str.length > 0) Set(str charAt 0) else set
          }
          
          p.first mustEqual first
        }
      }
      
      prop must pass
    }
  }
  
  "disjunctive parser" should {
    "parse binary alternatives" in {
      {
        val p = "daniel" | "chris"
        
        p("daniel" toStream) must beLike {
          case Success("daniel", Stream()) :: Nil => true
          case _ => false
        }
        
        p("chris" toStream) must beLike {
          case Success("chris", Stream()) :: Nil => true
          case _ => false
        }
        
        p("danieliel" toStream) must beLike {
          case Success("daniel", Stream('i', 'e', 'l')) :: Nil => true
          case _ => false
        }
      }
      
      {
        val p = "" | ""
        
        p("" toStream) must beLike {
          case Success("", Stream()) :: Nil => true
          case _ => false
        }
        
        p("" toStream) must beLike {
          case Success("", Stream()) :: Nil => true
          case _ => false
        }
        
        p("abc" toStream) must beLike {
          case Success("", Stream('a', 'b', 'c')) :: Nil => true
          case _ => false
        }
      }
    }
  }
}
