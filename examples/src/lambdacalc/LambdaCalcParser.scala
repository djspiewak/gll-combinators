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

package lambdacalc

import scala.collection.compat.immutable.LazyList
import scala.collection.mutable

import com.codecommit.gll._
import AST._

object LambdaCalcParser extends common.Example[(List[Alias], Expr)] with RegexParsers {

  // %%

  lazy val aliases = (alias+) ~ "-{3,}".r ~ expr ^^ { (a, _, e) => (a, e) }

  lazy val alias = x ~ "=" ~ expr                ^^ { (id, _, e) => Alias(id, e) }

  lazy val expr: Parser[Expr] = (
      "\\" ~> x ~ "." ~ expr      ^^ { (id, _, e) => Lambda(id, e) }
    | exprNoLam
  )

  lazy val exprNoLam: Parser[Expr] = (
      exprNoLam ~ exprNoApp       ^^ App
    | exprNoApp
  )

  lazy val exprNoApp = (
      "(" ~> expr <~ ")"
    | x                           ^^ Val
  )

  val x = """[^\s\.\\()=;]+""".r  ^^ { id => Symbol(id) }

  // %%

  def parser = aliases

  def handleSuccesses(forest: LazyList[(List[Alias], Expr)]): Unit = {
    val errors = mutable.Set[String]()

    val status = for ((tree, expr) <- forest) yield {
      try {
        val env = tree.foldLeft(scala.collection.mutable.Map():Env) { (env, alias) =>
          env(alias.id) = alias.expr.eval(env)
          env
        }

        Some(expr.eval(env))
      } catch {
        case EvalException(msg) => {
          errors += msg
          None
        }

        case _: StackOverflowError => {
          errors += "stack overflow"
          None
        }
      }
    }

    val results = status flatMap { x => x }

    if (results.length == 0) {
      for (msg <- errors) {
        println("  runtime error: " + msg)
      }
    } else if (results.length == 1)
      println("  " + results.head)
    else
      printf("  parse error: Ambiguous parse: %s valid trees%n", results.length.toString)
  }
}
