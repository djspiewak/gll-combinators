package edu.uwm.cs.gll

import scala.util.matching.Regex

sealed trait Result[+R] {
  val tail: LineStream
}

case class Success[+R](value: R, tail: LineStream) extends Result[R]

case class Failure(data: FailureData, tail: LineStream) extends Result[Nothing]


sealed trait FailureData

case class ExpectedLiteral(expect: String, received: String) extends FailureData

case class ExpectedRegex(regex: Regex) extends FailureData

case class UnexpectedEndOfStream(expected: Option[String]) extends FailureData

case class UnexpectedTrailingChars(received: String) extends FailureData

case class UnexpectedChars(received: String) extends FailureData

case object SyntaxError extends FailureData
