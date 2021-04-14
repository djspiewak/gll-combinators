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

import com.codecommit.gll.LazyLineCons
import com.codecommit.gll.LineStream

import scala.collection.compat.immutable.LazyList
import scala.io.Source

import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.scalacheck._

class LineStreamSpecs extends Specification with ScalaCheck {
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
      def allNums(ls: LineStream): LazyList[(Int, Int)] = {
        if (ls.isEmpty)
          LazyList.empty
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
