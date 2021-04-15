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

package common

import scala.collection.compat.immutable.LazyList
import scala.io.Source
import com.codecommit.gll._

trait Example[A] extends Parsers {

  def main(args: Array[String]): Unit = {
    for (file <- args) {
      println(file)
      println("=============================")

      val results = parser(LineStream(Source fromFile file))

      if (results exists { _.isInstanceOf[Success[A]] }) {
        handleSuccesses(results collect { case Success(tree, _) => tree })
      } else {
        val sorted = results.toList sortWith { _.tail.length < _.tail.length }
        val length = sorted.head.tail.length

        sorted.takeWhile(_.tail.length == length) collect {
          case Failure(msg, tail) =>
            val pattern = "  error:%%d: %s%n    %%s%n    %%s%n".format(msg)
            tail.printError(pattern)(System.err)
        }
      }

      println()
    }
  }

  def test(file: String): Boolean = {
    val src = Source.fromInputStream(getClass().getResourceAsStream(file))
    val results: LazyList[Result[A]] = parser(LineStream(src))
    if (results exists { _.isInstanceOf[Success[A]] }) {
      // suppress stdout:
      val stream = new java.io.ByteArrayOutputStream()
      Console.withOut(stream) {
        handleSuccesses(results collect { case Success(tree, _) => tree })
      }
      return true
    } else {
      return false
    }
  }

  def parser: Parser[A]

  def handleSuccesses(forest: LazyList[A]): Unit
}
