object StreamUtils {
  implicit def streamToRichStream[A](str: =>Stream[A]): RichStream[A] = new RichStream(str)

  class RichStream[A](str: =>Stream[A]) {
    def #::[B](hd: B) = Stream.cons(hd, str)

    def sort(pred: (A, A) => Boolean) = str.toList sortWith pred toStream    // cheating!
  }

  val SNil = Stream.empty

  object #:: {
    def unapply[A](str: Stream[A]): Option[(A, Stream[A])] =
      if (str.isEmpty) None else Some((str.head, str.tail))
  }
}
