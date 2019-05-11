/*
 * Copyright (c) 2019, Daniel Spiewak
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

import com.codecommit.gll._

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.specs2.specification.SpecificationFeatures
import org.scalacheck._

object TerminalSpecs extends Spec
    with SpecificationFeatures
    with ScalaCheck
    with Parsers {

  import Prop._
  import StreamUtils._

  "terminal parser" should {
    "parse single tokens" in {
      val p = literal("test")

      p("test") must beLike {
        case Success("test", LineStream()) #:: SNil => ok
      }
    }

    "produce 'expected' failure message" in {
      val p = literal("foo")

      p("bar") must beLike {
        case Failure(ExpectedLiteral("foo", "bar"), LineStream('b', 'a', 'r')) #:: SNil => ok
      }

      p("test") must beLike {
        case Failure(ExpectedLiteral("foo", "tes"), LineStream('t', 'e', 's', 't')) #:: SNil => ok
      }
    }

    "canonicalize failure message" in {
      val p = literal("")

      p("\n") must beLike {
        case Failure(UnexpectedTrailingChars("\\n"), LineStream('\n')) #:: SNil => ok
      }

      val p2 = literal("a")

      p2("\n") must beLike {
        case Failure(ExpectedLiteral("a", "\\n"), LineStream('\n')) #:: SNil => ok
      }

      p2("\r") must beLike {
        case Failure(ExpectedLiteral("a", "\\r"), LineStream('\r')) #:: SNil => ok
      }

      p2("\t") must beLike {
        case Failure(ExpectedLiteral("a", "\\t"), LineStream('\t')) #:: SNil => ok
      }

      p2("\f") must beLike {
        case Failure(ExpectedLiteral("a", "\\f"), LineStream('\f')) #:: SNil => ok
      }
    }

    "detect an unexpected end of stream" in {
      val p = literal("foo")

      p(LineStream('f')) must beLike {
        case Failure(UnexpectedEndOfStream(Some("foo")), LineStream('f')) #:: SNil => ok
      }

      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(Some("foo")), LineStream()) #:: SNil => ok
      }
    }

    "parse the empty string" in {
      val p = literal("")

      p(LineStream()) must beLike {
        case Success("", LineStream()) #:: SNil => ok
      }
    }

    "compute FIRST set" in {
      import com.codecommit.util.UniversalCharSet

      forAll { s: String =>
        if (s.length == 0)
          literal(s).first eq UniversalCharSet
        else
          literal(s).first == Set(s charAt 0)
      }
    }

    "map results according to a function" in {
      val p = "test" ^^ { _.length }

      p("test") must beLike {
        case Success(4, LineStream()) #:: SNil => ok
      }
    }

    "map results according to a value" in {
      val p = "test" ^^^ 42

      p("test") must beLike {
        case Success(42, LineStream()) #:: SNil => ok
      }
    }

    "map results with stream tail" in {
      var in1: LineStream = null
      val p1 = "foo" ^# { (in, s) =>
        in1 = in
        s
      }

      var in2: LineStream = null
      val p2 = "bar" ^# { (in, s) =>
        in2 = in
        s
      }

      val p = p1 ~ "\n" ~> p2

      p("foo\nbar") must beLike {
        case Success("bar", LineStream()) #:: SNil => ok
      }

      in1.line mustEqual "foo"
      in1.lineNum mustEqual 1
      in1.head mustEqual 'f'
      in1.toString mustEqual "foo\nbar"

      in2.line mustEqual "bar"
      in2.lineNum mustEqual 2
      in2.head mustEqual 'b'
      in2.toString mustEqual "bar"
    }
  }

  "terminal sequential parser" should {
    "parse multiple tokens" in {
      val p = "te" ~ "st"

      p("test") must beLike {
        case Success("te" ~ "st", LineStream()) #:: SNil => ok
      }
    }

    "produce 'expected' error message" in {
      val p = "te" ~ "st"

      p("foo") must beLike {
        case Failure(ExpectedLiteral("te", "fo"), LineStream('f', 'o', 'o')) #:: SNil => ok
      }

      p("tefoo") must beLike {
        case Failure(ExpectedLiteral("st", "fo"), LineStream('f', 'o', 'o')) #:: SNil => ok
      }
    }

    "detect an unexpected end of stream" in {
      val p = "te" ~ "st"

      p(LineStream('t')) must beLike {
        case Failure(UnexpectedEndOfStream(Some("te")), LineStream('t')) #:: SNil => ok
      }

      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(Some("te")), LineStream()) #:: SNil => ok
      }

      p("tes") must beLike {
        case Failure(UnexpectedEndOfStream(Some("st")), LineStream('s')) #:: SNil => ok
      }

      p("te") must beLike {
        case Failure(UnexpectedEndOfStream(Some("st")), LineStream()) #:: SNil => ok
      }
    }

    "compute FIRST set" in forAll { strs: List[String] =>
      import com.codecommit.util.UniversalCharSet

      (strs.length > 0 && (strs exists { _.length > 0 })) ==> {
        val p = strs.map(literal).reduceLeft[Parser[Any]] { _ ~ _ }

        val composite = strs.mkString
        val first = if (composite.length == 0) UniversalCharSet else Set(composite charAt 0)

        if (p.first.size == 0 && first.size == 0)
          true
        else
          p.first == first    // TODO file bug report
      }
    }
  }
}
