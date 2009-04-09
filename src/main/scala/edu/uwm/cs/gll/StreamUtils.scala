package edu.uwm.cs.gll

private[gll] object StreamUtils {
  object #:: {
    def unapply[A](str: Stream[A]) = str match {
      case Stream.cons(hd, tail) => Some((hd, tail))
      case Stream.empty => None
    }
  }
  
  implicit def streamSyntax[A](str: =>Stream[A]) = new {
    def #::[B](v: B) = Stream.cons(v, str)
  }
}
