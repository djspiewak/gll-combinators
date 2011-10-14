import edu.uwm.cs.gll.LazyLineCons
import edu.uwm.cs.gll.LineStream

import scala.io.Source

import org.specs._
import org.scalacheck._

object LineStreamSpecs extends Specification with ScalaCheck {
  import Prop._
  
  "LineStream" should {
    "have length for String construction" in {
      val prop = forAll { str: String =>
        LineStream(str).length == str.length
      }
      
      prop must pass
    }
    
    "have length for Source construction" in {
      val prop = forAll { str: String =>
        LineStream(Source.fromString(str)).length == str.length
      }
      
      prop must pass
    }
    
    "have length for Char* construction" in {
      val prop = forAll { str: String =>
        LineStream(str: _*).length == str.length
      }
      
      prop must pass
    }
    
    "correctly define apply()" in {
      val prop = forAll { (i: Int, str: String) =>
        (str.length > 0) ==> {
          val idx = abs(i % str.length)
          val stream = LineStream(str)
          
          stream(0) mustEqual stream.head
          stream(idx) mustEqual str(idx)
        }
      }
      
      prop must pass
    }
    
    "define equality by identity and not contents" in {
      val a = new LazyLineCons('a', fail(), "a", 1, 1)
      val b = new LazyLineCons('a', fail(), "a", 1, 1)
      
      (a == b) mustBe false        // not sure why mustEqual does bad things; bug in Specs?
      (a == a) mustBe true        // not sure why mustEqual does bad things; bug in Specs?
      (b == b) mustBe true        // not sure why mustEqual does bad things; bug in Specs?
    }
    
    "define hashCode by identity" in {
      new LazyLineCons('a', fail(), "a", 1, 1).hashCode mustEqual 1
    }
    
    "define a different lineNum/colNum pair for each index" in {
      def allNums(ls: LineStream): Stream[(Int, Int)] = {
        if (ls.isEmpty)
          Stream.empty
        else
          (ls.lineNum, ls.colNum) #:: allNums(ls.tail)
      }
      
      val prop = forAll { str: String =>
        val ls = LineStream(str)
        Set(allNums(ls): _*) must haveSize(str.length)
      }
      
      prop must pass
    }
  }
  
  def abs(i: Int) = Math.abs(i)
}
