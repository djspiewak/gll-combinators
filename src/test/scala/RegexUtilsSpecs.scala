/*
 * Copyright (c) 2021, Daniel Spiewak
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import com.codecommit._
import gll._
import util._

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

class RegexUtilsSpecs extends Specification with ScalaCheck {
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
