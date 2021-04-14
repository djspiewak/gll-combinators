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

package com.codecommit.gll

import com.codecommit.util._

import scala.util.matching.Regex

trait RegexParsers extends Parsers {
  import SetSyntax._

  protected val whitespace = """\s+"""r
  protected val skipWhitespace = true

  override implicit def literal(str: String): LiteralParser = {
    if (skipWhitespace) {
      new LiteralParser(str) {
        override def computeFirst(seen: Set[Parser[Any]]) = {
          val wsFirst = if (skipWhitespace) RegexUtils.first(whitespace) else Set[Option[Char]]()
          Some((wsFirst - None) ++ (super.computeFirst(seen) getOrElse Set[Option[Char]]()))
        }
      }
    } else super.literal(str)
  }

  implicit def regex(r: Regex): RegexParser =
    new RegexParser(r)

  override implicit def disjunctiveLiterals(left: String): RichRegexParser =
    new RichRegexParser(regex(new Regex(escapeRegex(left))))

  implicit def disjunctiveRegex(left: Regex): RichRegexParser =
    new RichRegexParser(left)

  implicit def funRegexSyntax(p: Regex) = new RichSyntax1(regex(p))

  private def escapeRegex(str: String) = {
    val specialChars = Set('[', ']', '{', '}', '\\', '|', '*', '+', '?', '^', '$', '(', ')')

    augmentString(str).foldLeft("") { (str, c) =>
      if (specialChars contains c)
        str + '\\' + c
      else
        str + c
    }
  }

  override protected def handleWhitespace(s: LineStream) =
    if (skipWhitespace)
      s.drop(whitespace findPrefixOf s map { _.length } getOrElse 0)
    else
      s


  class RichRegexParser(left: RegexParser) extends RichParser(left) {
    def |(right: Regex) = new RegexParser(new Regex("(" + left.regex.toString + ")|(" + right.regex.toString + ")"))

    def |(right: String) = new RegexParser(new Regex("(" + left.regex.toString + ")|(" + escapeRegex(right) + ")"))
  }

  case class RegexParser(val regex: Regex) extends TerminalParser[String] {
    if (regex == null)
      throw new NullPointerException("Cannot parse a null regular expression")

    lazy val constFirst = {
      val wsFirst = if (skipWhitespace) RegexUtils.first(whitespace) else Set[Option[Char]]()
      val regexFirst = RegexUtils.first(regex)

      if (wsFirst.isComplement)
        Some((wsFirst - None) ++ regexFirst)
      else
        Some(regexFirst ++ (wsFirst - None))
    }

    def computeFirst(s: Set[Parser[Any]]) = constFirst

    def parse(in: LineStream) = {
      val res = regex findPrefixOf in map { str => Success(str, in drop str.length) }
      res getOrElse Failure(ExpectedRegex(regex), in)
    }

    override def equals(other: Any) = other match {
      case that: RegexParser => this.regex.toString == that.regex.toString
      case _ => false
    }

    override def hashCode = regex.toString.hashCode

    override def toString = "/%s/".format(regex)
  }
}

object RegexParsers extends RegexParsers
