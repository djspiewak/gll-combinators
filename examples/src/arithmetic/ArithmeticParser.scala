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

package arithmetic

import com.codecommit.gll._
import AST._

object ArithmeticParser extends common.Example[Expr] with RegexParsers {
  
  // %%
  
  lazy val expr: Parser[Expr] = (
      expr ~ "+" ~ term     ^^ { (e1, _, e2) => Add(e1, e2) }
    | expr ~ "-" ~ term     ^^ { (e1, _, e2) => Sub(e1, e2) }
    | term
  )
  
  lazy val term: Parser[Expr] = (
      term ~ "*" ~ factor   ^^ { (e1, _, e2) => Mul(e1, e2) }
    | term ~ "/" ~ factor   ^^ { (e1, _, e2) => Div(e1, e2) }
    | factor
  )
  
  lazy val factor: Parser[Expr] = (
      "(" ~> expr <~ ")"
    | "-" ~> factor         ^^ Neg
    | """\d+""".r           ^^ { str => IntLit(str.toInt) }
  )
  
  // %%
  
  def parser = expr
  
  def handleSuccesses(forest: Stream[Expr]) {
    forest foreach println
  }
}
