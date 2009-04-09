package edu.uwm.cs.gll

sealed trait Result[+R] {
  val tail: Stream[Char]
}

case class Success[+R](value: R, tail: Stream[Char]) extends Result[R]

case class Failure(msg: String, tail: Stream[Char]) extends Result[Nothing]
