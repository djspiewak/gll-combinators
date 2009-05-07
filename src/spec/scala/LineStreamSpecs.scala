import edu.uwm.cs.gll.LineStream

import scala.io.Source

import org.specs._
import org.scalacheck._

object LineStreamSpecs extends Specification with ScalaCheck {
  import Prop._
  
  "LineStream" should {
    "have length for String construction" in {
      val prop = forAll { str: String =>
        LineStream(str).length mustBe str.length
      }
      
      prop must pass
    }
    
    "have length for Source construction" in {
      val prop = forAll { str: String =>
        LineStream(Source.fromString(str)).length mustBe str.length
      }
      
      prop must pass
    }
    
    "have length for Char* construction" in {
      val prop = forAll { str: String =>
        LineStream(str:_*).length mustBe str.length
      }
      
      prop must pass
    }
    
    "correctly define apply()" in {
      val prop = forAll { (i: Int, str: String) =>
        (0 until str.length).contains(i) ==> {
          val stream = LineStream(str)
          
          stream(0) mustEqual stream.head
          stream(i) mustEqual str(i)
        }
      }
      
      prop must pass
    }
  }
}
