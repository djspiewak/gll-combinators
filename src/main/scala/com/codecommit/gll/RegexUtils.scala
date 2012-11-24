package com.codecommit.gll

import com.codecommit.util._

import scala.util.matching.Regex

object RegexUtils extends Parsers {    // note, *not* RegexParsers!
  import SetSyntax._

  def first(regex: Regex): Set[Option[Char]] = {
    val results = full(regex.toString) flatMap {
      case Success(t, _) => t.first :: Nil
      case _ => Nil
    } toList

    if (results.isEmpty) {
      UniversalOptCharSet
    } else {
      val sorted = results sortWith { (a, b) => a.isComplement || !b.isComplement }    // sort complements first
      sorted reduceLeft { _ ++ _ }
    }
  }


  // %%

  private lazy val full: Parser[Token] = disj <~ ("\\b"?)

  private lazy val disj: Parser[Token] = (
      disj ~ ("|" ~> seq)    ^^ DisjToken
    | seq
  )

  private lazy val seq: Parser[Token] = (
      seq ~ rep    ^^ SeqToken
    | rep
  )

  private lazy val rep: Parser[Token] = (
      token ~ "{" ~ num ~ "," ~ (num?) ~ "}"    ^^ { (t, _, from, _, to, _) => RepToken(t, from, to) }
    | rep <~ "+"                                ^^ { RepToken(_, 1, None) }
    | rep <~ "?"                                ^^ OptToken
    | rep <~ "*"                                ^^ { RepToken(_, 0, None) }
    | token
  )

  private lazy val token: Parser[Token] = (
      "(" ~> disj <~ ")"
    | "[" ~> charClass <~ "]"
    | "\\" ~> escape
    | charValue                 ^^ CharClass
    | "."                      ^^^ AnyChar
  )

  private lazy val charClass = (
      "^" ~> classContents   ^^ NegationClass
    | classContents
  )

  private lazy val classContents: Parser[ClassToken] = (
      classContents ~ classValue  ^^ MultiClass
    | classValue
  )

  private lazy val classValue: Parser[ClassToken] = (
      charValue                       ^^ CharClass
    | "("                            ^^^ CharClass('(')
    | ")"                            ^^^ CharClass(')')
    | "{"                            ^^^ CharClass('{')
    | "}"                            ^^^ CharClass('}')
    | "\\" ~> escape
    | charValue ~ ("-" ~> charValue)  ^^ RangeClass
    | "."                            ^^^ AnyChar
  )

  private val escape = (
      "d"  ^^^ NumberClass
    | "D"  ^^^ NegationClass(NumberClass)
    | "w"  ^^^ AlphaClass
    | "W"  ^^^ NegationClass(AlphaClass)
    | "s"  ^^^ WhitespaceClass
    | "S"  ^^^ NegationClass(WhitespaceClass)
    | "n"  ^^^ CharClass('\n')
    | "r"  ^^^ CharClass('\r')
    | "t"  ^^^ CharClass('\t')
    | "\\" ^^^ CharClass('\\')
    | "$"  ^^^ CharClass('$')
    | "^"  ^^^ CharClass('^')
    | "-"  ^^^ CharClass('-')
    | "("  ^^^ CharClass('(')
    | ")"  ^^^ CharClass(')')
    | "."  ^^^ CharClass('.')
    | "["  ^^^ CharClass('[')
    | "]"  ^^^ CharClass(']')
    | "{"  ^^^ CharClass('{')
    | "}"  ^^^ CharClass('}')
    | "|"  ^^^ CharClass('|')
    | "+"  ^^^ CharClass('+')
    | "*"  ^^^ CharClass('*')
    | "?"  ^^^ CharClass('?')
  )

  private val charValue = new TerminalParser[Char] {
    val specialChars = Set('[', ']', '{', '}', '\\', '|', '*', '+', '?', '^', '$', '(', ')', '.')

    def computeFirst(seen: Set[Parser[Any]]) = Some(UniversalOptCharSet)            // Some((specialChars map { Some(_): Option[Char] }).complement - None)

    def parse(in: LineStream) = {
      if (in.isEmpty)
        Failure(UnexpectedEndOfStream(None), in)
      else if (specialChars contains in.head)
        Failure(SyntaxError, in)
      else
        Success(in.head, in drop 1)
    }
  }

  private val num = ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") ^^ { _.toInt }

  // tokens

  private sealed trait Token {
    val first: Set[Option[Char]]        // set will include None iff regex is nullable
  }

  private case class RepToken(token: Token, from: Int, to: Option[Int]) extends Token {
    val first = {
      if (from == 0)
        Set(None: Option[Char])
      else
        token.first
    }
  }

  private case class OptToken(token: Token) extends Token {
    val first = Set(None: Option[Char])
  }

  private case class SeqToken(left: Token, right: Token) extends Token {
    val first = left.first
  }

  private case class DisjToken(left: Token, right: Token) extends Token {
    val first = {
      if (right.first.isComplement)
        right.first ++ left.first
      else
        left.first ++ right.first
    }
  }


  // character classes

  private sealed trait ClassToken extends Token

  private case object AnyChar extends ClassToken {
    val first = UniversalOptCharSet -- WhitespaceClass.first
  }

  private case object NumberClass extends ClassToken {
    val first = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9') map { Some(_): Option[Char] }
  }

  private case object WhitespaceClass extends ClassToken {
    val first = Set(' ', '\t', '\r', '\n') map { Some(_): Option[Char] }
  }

  private case object AlphaClass extends ClassToken {
    val first = Set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ": _*) map { Some(_): Option[Char] }
  }

  private case class RangeClass(start: Char, end: Char) extends ClassToken {
    val first = Set(start.toInt to end.toInt map { _.toChar }: _*) map { Some(_): Option[Char] }
  }

  private case class CharClass(c: Char) extends ClassToken {
    val first = Set(Some(c): Option[Char])
  }

  private case class MultiClass(left: ClassToken, right: ClassToken) extends ClassToken {
    val first = {
      if (right.first.isComplement)
        right.first ++ left.first
      else
        left.first ++ right.first
    }
  }

  private case class NegationClass(inner: ClassToken) extends ClassToken {
    val first = inner.first.complement - None
  }
}
