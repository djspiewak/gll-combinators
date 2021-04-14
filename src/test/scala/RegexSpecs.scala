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

import scala.collection.compat.immutable.LazyList
import scala.collection.compat.immutable.LazyList._
import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.specs2.specification.SpecificationFeatures

class RegexSpecs extends Spec
    with SpecificationFeatures
    with ScalaCheck
    with RegexParsers {

  "regex parsers" should {
    "match, consume and return from a regex match" in {
      {
        val p: Parser[String] = """(\d{1,3}\.){3}\d{1,3}"""r

        p("192.168.101.2") must beLike {
          case Success("192.168.101.2", LineStream()) #:: LazyList() => ok
        }
      }

      {
        val p: Parser[String] = """\d+"""r

        p("1234daniel") must beLike {
          case Failure(UnexpectedTrailingChars("daniel"), LineStream('d', 'a', 'n', 'i', 'e', 'l')) #:: LazyList() => ok
        }
      }
    }

    "properly escape contained strings turned into regexes" in {
      val p: Parser[String] = "(" | ")"

      p("(") must beLike {
        case Success("(", LineStream()) #:: LazyList() => ok
      }

      p(")") must beLike {
        case Success(")", LineStream()) #:: LazyList() => ok
      }
    }

    "match correctly when using a boundary token" in {
      val p = "a" ~ "where\\b".r ~ "b"

      p("a where b") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }

      p("a whereb") must beLike {
        case Failure(_, _) #:: LazyList() => ok
      }
    }

    "gracefully error on null regexp" in {
      regex(null) must throwA(new NullPointerException("Cannot parse a null regular expression"))
    }

    "produce 'expected' error message" in {
      val rex = """(\d{1,3}\.){3}\d{1,3}"""r
      val p: Parser[String] = rex

      {
        val data = LineStream("123.457.321")

        p(data) must beLike {
          case Failure(ExpectedRegex(`rex`), `data`) #:: LazyList() => ok
        }
      }

      {
        val data = LineStream("123.457.321.sdf")

        p(data) must beLike {
          case Failure(ExpectedRegex(`rex`), `data`) #:: LazyList() => ok
        }
      }

      {
        p(LineStream()) must beLike {
          case Failure(ExpectedRegex(`rex`), LineStream()) #:: LazyList() => ok
        }
      }
    }

    "produce a location of after leading whitespace" in {
      case class A(loc: LineStream, x: String)

      val p = literal("daniel") ^# { (loc, x) => A(loc, x) }

      p("    daniel") must beLike {
        case Success(A(l, "daniel"), LineStream()) #:: LazyList() =>
          l.colNum mustEqual 5
          l.toString mustEqual "daniel"
      }
    }

    "produce a location of after leading whitespace in seqential parser" in {
      sealed trait A
      case class B(loc: LineStream, id: String, left: String, right: A) extends A
      case class C(loc: LineStream, id: String) extends A

      lazy val p: Parser[A] = (
          "daniel" ~ ":=" ~ "daniel" ~ p ^# { (loc, id, _, a, b) => B(loc, id, a, b) }
        | "daniel"                       ^# { (loc, d) => C(loc, d) }
      )

      p("\n\n\n\n daniel := daniel daniel") must beLike {
        case Success(B(l, "daniel", "daniel", C(_, "daniel")), LineStream()) #:: LazyList() =>
          l.lineNum mustEqual 5
          l.colNum mustEqual 2
      }
    }

    "eat leading whitespace" in {
      val p = literal("daniel")

      p("daniel") must beLike {
        case Success("daniel", LineStream()) #:: LazyList() => ok
      }

      p("  daniel") must beLike {
        case Success("daniel", LineStream()) #:: LazyList() => ok
      }

      p("\tdaniel") must beLike {
        case Success("daniel", LineStream()) #:: LazyList() => ok
      }

      p("""
      daniel""") must beLike {
        case Success("daniel", LineStream()) #:: LazyList() => ok
      }
    }

    "eat trailing whitespace" in {
      val p = literal("daniel")

      p("daniel    ") must beLike {
        case Success("daniel", LineStream()) #:: LazyList() => ok
      }

      p("daniel\t") must beLike {
        case Success("daniel", LineStream()) #:: LazyList() => ok
      }

      p("daniel\n") must beLike {
        case Success("daniel", LineStream()) #:: LazyList() => ok
      }
    }

    "eat infix whitespace" in {
      val num = """\d+""".r
      val p = num ~ "+" ~ num

      p("1 + 2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }

      p("1 +2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }

      p("1+ 2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }

      p("  1   +  2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }
    }

    "eat infix whitespace with explicit type attribution" in {
      val num: Parser[String] = """\d+""".r
      val p = num ~ "+" ~ num

      p("1 + 2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }

      p("1 +2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }

      p("1+ 2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }

      p("  1   +  2") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }
    }

    "eat infix whitespace without disrupting subsquent regex" in {
      val num = """[0-9]+(\.[0-9]+)?([eE][0-9]+)?""".r
      val str = """"([^\n\r\\]|\\.)*"""".r

      lazy val expr: Parser[Any] = (
          expr ~ "+" ~ expr
        | num
        | str
      )

      expr("1 + \"a\"") must beLike {
        case Success(_, LineStream()) #:: _ => ok
      }
    }

    "define FIRST set" in {
      """\d""".r.first must containAllOf(List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'))
      "abc".r.first must contain('a')
      "abc|def".r.first must containAllOf(List('a', 'd'))
      ("a*".r.first eq UniversalCharSet) mustEqual true
      "a+".r.first must contain('a')
      ("[^abc]".r.first == new ComplementarySet(Set('a', 'b', 'c'))) mustEqual true
      ("abc|(def)|[^abc]".r.first == new ComplementarySet(Set('b', 'c'))) mustEqual true
    }

    "negate using a regexp parser" in {
      val p1 = "test" \ ("test" | "ing")

      p1("test") must beLike {
        case Failure(SyntaxError, LineStream(tail @ _*)) #:: LazyList() =>
          tail.mkString mustEqual "test"
      }

      val p2 = "test" \ ("blah" | "ing ")

      p2("test") must beLike {
        case Success("test", LineStream()) #:: LazyList() => ok
      }

      p2("ing ") must beLike {
        case Failure(ExpectedLiteral("test", "ing "), LineStream(tail @ _*)) #:: LazyList() =>
          tail.mkString mustEqual "ing "
      }

      p2("blah") must beLike {
        case Failure(ExpectedLiteral("test", "blah"), LineStream(tail @ _*)) #:: LazyList() =>
          tail.mkString mustEqual "blah"
      }
    }

    "correctly globally disambiguate a local sequence ambiguity" in {
      lazy val expr: Parser[Any] = (
          id ~ ":=" ~ expr ~ expr
        | num
        | id
      )

      lazy val id = """[a-zA-Z][a-zA-Z0-9_]*""".r
      lazy val num = """\d+""".r

      expr("a := 1 c := 2 3") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }
    }

    "not double-escape a disjuncted regex on the RHS" in {
      val pat = "_" | """[a-z]+""".r

      pat("long") must beLike {
        case Success(_, LineStream()) #:: LazyList() => ok
      }
    }
  }
}
