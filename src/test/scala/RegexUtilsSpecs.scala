import com.codecommit._
import gll._
import util._

import org.specs._
import org.scalacheck._

object RegexUtilsSpecs extends Specification with ScalaCheck {
  import Prop._
  import RegexUtils._
  
  "regexp FIRST set computation" should {
    val specialChars = Set('[', ']', '{', '}', '\\', '|', '*', '+', '?', '^', '$', '(', ')', '.')
    
    "handle single characters" in {
      val prop = forAll { c: Char =>
        !specialChars(c) ==> {
          first(c.toString.r) contains Some(c)
        }
      }
      
      prop must pass
    }
    
    "handle character sequences" in {
      val prop = forAll { str: String =>
        (!(str exists specialChars) && !str.isEmpty) ==> {
          first(str.r) contains Some(str charAt 0)
        }
      }
      
      prop must pass
    }
    
    "handle disjunctions" in {
      first("a|b".r) must containAll(Set(Some('a'), Some('b')))
      first("a|b|c".r) must containAll(Set(Some('a'), Some('b'), Some('c')))
      first("a|b|c|a".r) must containAll(Set(Some('a'), Some('b'), Some('c')))
    }
    
    "handle regex ending in \\b" in {
      first("abc\\b".r) must containAll(Set(Some('a')))
    }
    
    "handle all special character classes" in {
      val alpha = Set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ": _*) map { Some(_): Option[Char] }
      val num = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9') map { Some(_): Option[Char] }
      val ws = Set(' ', '\t', '\r', '\n') map { Some(_): Option[Char] }
      
      first("""\d"""r) must containAll(num)
      first("""\D"""r) mustEqual (new ComplementarySet(num) - None)
      
      first("""\w"""r) must containAll(alpha)
      first("""\W"""r) mustEqual (new ComplementarySet(alpha) - None)
      
      first("""\s"""r) must containAll(ws)
      first("""\S"""r) mustEqual (new ComplementarySet(ws) - None)
      
      first("""\n"""r) must containAll(Set(Some('\n')))
      first("""\r"""r) must containAll(Set(Some('\r')))
      first("""\t"""r) must containAll(Set(Some('\t')))
    }
    
    "correctly parse a char set containing otherwise-illegal characters" in {
      RegexUtils.first("[(]".r) mustEqual Set(Some('('))
      RegexUtils.first("[)]".r) mustEqual Set(Some(')'))
      RegexUtils.first("[{]".r) mustEqual Set(Some('{'))
      RegexUtils.first("[}]".r) mustEqual Set(Some('}'))
    }
    
    "return the universal set for a failed parse" in {
      first("""\@"""r) mustBe UniversalOptCharSet
    }
  }
}
