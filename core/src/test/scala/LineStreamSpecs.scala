import com.codecommit.gll.LazyLineCons
import com.codecommit.gll.LineStream

import scala.io.Source

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

object LineStreamSpecs extends Specification with ScalaCheck {
  import Function._
  import Prop._

  "LineStream" should {
    "have length for String construction" in forAll { str: String =>
      LineStream(str).length mustEqual str.length
    }

    "have length for Source construction" in forAll { str: String =>
      LineStream(Source.fromString(str)).length mustEqual str.length
    }

    "have length for Char* construction" in forAll { str: String =>
      LineStream(str: _*).length mustEqual str.length
    }

    "correctly define apply()" in forAll { (i: Int, str: String) =>
      (str.length > 0) ==> {
        val idx = abs(i % str.length)
        val stream = LineStream(str)

        stream(0) mustEqual stream.head
        stream(idx) mustEqual (str charAt idx)
      }
    }

    "define equality by identity and not contents" in {
      val a = new LazyLineCons('a', sys.error("clever test failure"), "a", 1, 1)
      val b = new LazyLineCons('a', sys.error("clever test failure"), "a", 1, 1)

      (a == b) mustEqual false
      (a == a) mustEqual true
      (b == b) mustEqual true
    }

    "define hashCode by identity" in {
      new LazyLineCons('a', sys.error("clever test failure"), "a", 1, 1).hashCode mustEqual 1
    }

    "define a different lineNum/colNum pair for each index" in {
      def allNums(ls: LineStream): Stream[(Int, Int)] = {
        if (ls.isEmpty)
          Stream.empty
        else
          (ls.lineNum, ls.colNum) #:: allNums(ls.tail)
      }

      forAll { str: String =>
        val ls = LineStream(str)
        Set(allNums(ls): _*) must haveSize(str.length)
      }
    }

    "not throw SOE on large input" in {
      val str = (0 until 1000000) map const('a') mkString "\n"

      LineStream(str).length must not(throwA[StackOverflowError])
    }
  }

  def abs(i: Int) = math.abs(i)
}
