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

object DisjunctionSpecs extends Spec
    with SpecificationFeatures
    with Parsers
    with ScalaCheck {

  import Prop._
  import StreamUtils._

  "disjunctive parser" should {
    "detect LL(1) grammar" in {
      {
        val p = "daniel" | "chris" | "joseph"
        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustEqual true
      }

      {
        lazy val p: Parser[String] = (
            "a" ~ p ^^ { _ + _ }
          | "b"
        )
        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustEqual true
      }

      {
        lazy val p: Parser[String] = (
            "a" ~ p ^^ { _ + _ }
          | "a"
        )

        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustEqual false
      }

      {
        lazy val p: Parser[String] = (
            p ~ "b" ^^ { _ + _ }
          | "a"
        )

        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustEqual false
      }

      {
        lazy val p: Parser[String] = (
            "b" ~ p ^^ { _ + _ }
          | ""
        )

        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustEqual false
      }
    }

    "gather binary alternatives" in forAll { (left: String, right: String) =>
      val p = (left | right).asInstanceOf[DisjunctiveParser[String]]
      p.gather mustEqual List(literal(left), literal(right))
    }

    "compute FIRST for binary alternatives" in forAll { (left: String, right: String) =>
      import com.codecommit.util._

      val leftFirst = if (left.length == 0) Set[Char]() else Set(left charAt 0)
      val rightFirst = if (right.length == 0) Set[Char]() else Set(right charAt 0)
      if (leftFirst.size == 0 || rightFirst.size == 0)
        (left | right).first eq UniversalCharSet
      else
        (left | right).first == (leftFirst ++ rightFirst)
    }

    "parse binary alternatives" in {
      {
        val p = "daniel" | "chris"

        p("daniel") must beLike {
          case Success("daniel", LineStream()) #:: SNil => ok
        }

        p("chris") must beLike {
          case Success("chris", LineStream()) #:: SNil => ok
        }
      }

      {
        val p = "" | ""

        p("") must beLike {
          case Success("", LineStream()) #:: SNil => ok
        }
      }
    }

    "detect PREDCIT failure for LL(1)" in {
      val p = "daniel" | "chris"

      p(LineStream('j')) must beLike {
        case Failure(UnexpectedChars("j"), LineStream('j')) #:: SNil => ok
      }

      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(None), LineStream()) #:: SNil => ok
      }
    }

    "detect PREDICT failure for non-LL(1)" in {
      val p = "daniel" | "danielle"

      p(LineStream('j')) must beLike {
        case Failure(UnexpectedChars("j"), LineStream('j')) #:: SNil => ok
      }

      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(None), LineStream()) #:: SNil => ok
      }
    }

    "produce binary failures for LL(1)" in {
      val p = "daniel" | "chris"

      p("dan") must beLike {
        case Failure(UnexpectedEndOfStream(Some("daniel")), LineStream('d', 'a', 'n')) #:: SNil => ok
      }

      p("dancin") must beLike {
        case Failure(ExpectedLiteral("daniel", "dancin"), LineStream('d', 'a', 'n', 'c', 'i', 'n')) #:: SNil => ok
      }
    }

    "canonicalize failure message" in {
      val p = literal("") | literal("")

      p("\n") must beLike {
        case Failure(UnexpectedTrailingChars("\\n"), LineStream('\n')) #:: SNil => ok
      }
    }

    "produce binary failures for non-LL(1)" in {
      val p = "foobar" | "foobaz"

      {
        val data = LineStream("foo")

        p(data) must containTheSameElementsAs(List(
          Failure(UnexpectedEndOfStream(Some("foobar")), data),
          Failure(UnexpectedEndOfStream(Some("foobaz")), data)))
      }

      {
        val data = LineStream("foobat")

        p(data) must containTheSameElementsAs(List(
          Failure(ExpectedLiteral("foobar", "foobat"), data),
          Failure(ExpectedLiteral("foobaz", "foobat"), data)))
      }
    }

    "gather nary alternatives" in {
      def check(p: Parser[Any], expected: Parser[String]*) = {
        p.asInstanceOf[DisjunctiveParser[String]].gather must containAllOf(expected)
      }

      {
        val p = "daniel" | "chris" | "joseph"
        check(p, "daniel", "chris", "joseph")
      }

      {
        val p = "daniel" | "daniel" | "chris" | "joseph"
        check(p, "daniel", "chris", "joseph")
      }

      {
        val p = "" | "chris" | "" | "daniel" | "daniel"
        check(p, "", "daniel", "chris")
      }
    }

    "compute FIRST for nary alternatives" in {
      import com.codecommit.util._

      ("daniel" | "chris" | "joseph").first mustEqual Set('d', 'c', 'j')
      ("daniel" | "daniel" | "chris" | "joseph").first mustEqual Set('d', 'c', 'j')
      (("" | "chris" | "" | "daniel" | "daniel").first eq UniversalCharSet) mustEqual true
    }

    "parse nary alternatives" in {
      // assumes unambiguous data
      def check(p: Parser[Any], data: String*) =
        data forall (str => p(str) must beLike { case Success(`str`, LineStream()) #:: SNil => ok })

      {
        val p = "daniel" | "chris" | "joseph" | "renee" | "bethany" | "grace"
        check(p, "daniel", "chris", "joseph", "renee", "bethany", "grace")
      }

      {
        val p = "renee" | "bethany" | "grace"
        check(p, "renee", "bethany", "grace")
      }

      {
        val p = "daniel" | "chris" | "joseph" | "renee"
        check(p, "daniel", "chris", "joseph", "renee")
      }
    }

    "produce nary failures for LL(1)" in {
      // assumes unambiguous data
      def check1(p: Parser[Any], expect: String*)(data: String*) = {
        data forall { str =>
          val stream = LineStream(str)
          val res = p(stream)

          val failures = expect.foldRight(List[String]()) { _ :: _ } map { ex =>
            Failure(ExpectedLiteral(ex, str), stream)
          }

          res must containTheSameElementsAs(failures)
        }
      }

      def check2(p: Parser[Any], expect: String*)(data: String*) = {
        data forall { str =>
          val stream = LineStream(str)
          val res = p(stream)

          val failures = expect.foldRight(List[String]()) { _ :: _ } map { ex =>
            Failure(UnexpectedEndOfStream(Some(ex)), stream)
          }

          res must containTheSameElementsAs(failures)
        }
      }

      {
        val p = "daniel" | "chris" | "joseph" | "renee" | "bethany" | "grace"

        check1(p, "daniel")("dancin")
        check1(p, "chris")("chari")
        check1(p, "joseph")("josefs")

        check2(p, "daniel")("dan", "d")
        check2(p, "joseph")("joe", "j")
        check2(p, "bethany")("beth", "b")
      }

      {
        val p = "renee" | "bethany" | "grace"

        check1(p, "renee")("rainb")
        check1(p, "bethany")("battiny")
        check1(p, "grace")("grabo")

        check2(p, "renee")("ren", "r")
        check2(p, "bethany")("beth", "b")
        check2(p, "grace")("gr", "g")
      }
    }

    "map results" in forAll { (left: String, right: String, f: String => Int) =>
      left != right ==> {
        val p = (
            left
          | right
        ) ^^ f

        p(left) must beLike {
          case Success(v, LineStream()) #:: SNil => v mustEqual f(left)
        }

        p(right) must beLike {
          case Success(v, LineStream()) #:: SNil => v mustEqual f(right)
        }
      }
    }

    "map results with stream tail" in {
      var in1: LineStream = null
      val p1 = ("foo" | "bar") ^# { (in, s) =>
        in1 = in
        s
      }

      var in2: LineStream = null
      val p2 = ("baz" | "bin") ^# { (in, s) =>
        in2 = in
        s
      }

      val p = p1 ~ "\n" ~> p2

      p("foo\nbaz") must beLike {
        case Success("baz", LineStream()) #:: SNil => ok
      }

      in1 must not(beNull)
      in1.line mustEqual "foo"
      in1.lineNum mustEqual 1
      in1.head mustEqual 'f'
      in1.toString mustEqual "foo\nbaz"

      in2 must not(beNull)
      in2.line mustEqual "baz"
      in2.lineNum mustEqual 2
      in2.head mustEqual 'b'
      in2.toString mustEqual "baz"


      p("foo\nbin") must beLike {
        case Success("bin", LineStream()) #:: SNil => ok
      }

      in1 must not(beNull)
      in1.line mustEqual "foo"
      in1.lineNum mustEqual 1
      in1.head mustEqual 'f'
      in1.toString mustEqual "foo\nbin"

      in2 must not(beNull)
      in2.line mustEqual "bin"
      in2.lineNum mustEqual 2
      in2.head mustEqual 'b'
      in2.toString mustEqual "bin"

      p("bar\nbaz") must beLike {
        case Success("baz", LineStream()) #:: SNil => ok
      }

      in1 must not(beNull)
      in1.line mustEqual "bar"
      in1.lineNum mustEqual 1
      in1.head mustEqual 'b'
      in1.toString mustEqual "bar\nbaz"

      in2 must not(beNull)
      in2.line mustEqual "baz"
      in2.lineNum mustEqual 2
      in2.head mustEqual 'b'
      in2.toString mustEqual "baz"

      p("bar\nbin") must beLike {
        case Success("bin", LineStream()) #:: SNil => ok
      }

      in1 must not(beNull)
      in1.line mustEqual "bar"
      in1.lineNum mustEqual 1
      in1.head mustEqual 'b'
      in1.toString mustEqual "bar\nbin"

      in2 must not(beNull)
      in2.line mustEqual "bin"
      in2.lineNum mustEqual 2
      in2.head mustEqual 'b'
      in2.toString mustEqual "bin"
    }

    "handle binary shift/reduce ambiguity" in forAll { (head: String, suffix: String) =>
      // %%

      val p1 = (
          head ~ suffix     ^^ { _ + _ }
        | head
      )

      val p2 = "" | suffix  ^^ { " " + _ }

      val p = p1 ~ p2       ^^ { _ + _ }

      // %%

      val result = p(head + suffix)

      result.length mustEqual 2

      result must contain(Success(head + suffix, LineStream()))
      result must contain(Success(head + " " + suffix, LineStream()))
    }

    "compute FIRST for left-recursive grammar" in {
      lazy val p: Parser[Any] = p ~ "a" | "a"

      p.first mustEqual Set('a')
    }

    "prefer an appropriately annotated terminal parser in case of ambiguity" in {
      lazy val p = (
          ("ab" preferred) ^^^ 3
        | "a" ~ "b"        ^^^ 2
      )

      p("ab") must beLike {
        case Stream(Success(3, LineStream())) => ok
      }
    }
  }
}
