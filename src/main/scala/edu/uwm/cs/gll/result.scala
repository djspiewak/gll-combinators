package edu.uwm.cs.gll

sealed trait Result[+R] {
  val tail: LineStream
}

case class Success[+R](value: R, tail: LineStream) extends Result[R]

case class Failure(msg: String, tail: LineStream) extends Result[Nothing]
