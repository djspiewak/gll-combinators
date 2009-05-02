package edu.uwm.cs.util

private[cs] object StreamUtils {
  import Stream._
    
  object #:: {
    def unapply[A](str: Stream[A]) = str match {
      case cons(hd, tail) => Some((hd, tail))
      case empty => None
    }
  }
  
  implicit def streamSyntax[A](str: =>Stream[A]) = new {
    def #::[B](v: B) = cons(v, str)
  }
  
  implicit def repSyntax[A](v: A) = new {
    def rep: Stream[A] = cons(v, rep)
  }
  
  implicit def str2stream(str: String) = new {
    def toProperStream = {
      def gen(i: Int): Stream[Char] = {
        if (i < str.length) 
          cons(str.charAt(i), gen(i + 1))
        else
          empty
      }
      
      gen(0)
    }
  }
}
