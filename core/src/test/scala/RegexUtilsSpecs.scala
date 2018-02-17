import com.codecommit._
import gll._
import util._

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

object RegexUtilsSpecs extends Specification with ScalaCheck {
  import Prop._
  import RegexUtils._

  "regexp FIRST set computation" should {
    val specialChars = Set('[', ']', '{', '}', '\\', '|', '*', '+', '?', '^', '$', '(', ')', '.')

    "handle single characters" in forAll { c: Char =>
      !specialChars(c) ==> {
        first(c.toString.r) must contain(Some(c))
      }
    }

    "handle character sequences" in forAll { str: String =>
      (!(str exists specialChars) && !str.isEmpty) ==> {
        first(str.r) must contain(Some(str charAt 0))
      }
    }

    "handle disjunctions" in {
      first("a|b".r) must containAllOf(List(Some('a'), Some('b')))
      first("a|b|c".r) must containAllOf(List(Some('a'), Some('b'), Some('c')))
      first("a|b|c|a".r) must containAllOf(List(Some('a'), Some('b'), Some('c')))
    }

    "handle regex ending in \\b" in {
      first("abc\\b".r) must containAllOf(List(Some('a')))
    }

    "handle all special character classes" in {
      val alpha = List("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ": _*) map { Some(_): Option[Char] }
      val num = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9') map { Some(_): Option[Char] }
      val ws = List(' ', '\t', '\r', '\n') map { Some(_): Option[Char] }

      first("""\d"""r) must containAllOf(num)
      (first("""\D"""r) == (new ComplementarySet(Set(num: _*)) - None)) mustEqual true

      first("""\w"""r) must containAllOf(alpha)
      (first("""\W"""r) == (new ComplementarySet(Set(alpha: _*)) - None)) mustEqual true

      first("""\s"""r) must containAllOf(ws)
      (first("""\S"""r) == (new ComplementarySet(Set(ws: _*)) - None)) mustEqual true

      first("""\n"""r) must containAllOf(List(Some('\n')))
      first("""\r"""r) must containAllOf(List(Some('\r')))
      first("""\t"""r) must containAllOf(List(Some('\t')))
    }

    "correctly parse a char set containing otherwise-illegal characters" in {
      RegexUtils.first("[(]".r) mustEqual Set(Some('('))
      RegexUtils.first("[)]".r) mustEqual Set(Some(')'))
      RegexUtils.first("[{]".r) mustEqual Set(Some('{'))
      RegexUtils.first("[}]".r) mustEqual Set(Some('}'))
    }

    "return the universal set for a failed parse" in {
      (first("""\@"""r) eq UniversalOptCharSet) mustEqual true
    }
  }
}
