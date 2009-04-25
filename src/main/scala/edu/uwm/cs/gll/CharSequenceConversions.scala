package edu.uwm.cs.gll

trait CharSequenceConversions {
  implicit def stream2CharSequence(s: Stream[Char]): CharSequence = new StreamCharSequence(s)
  
  /**
   * An amortized O(1) view of a character stream.  This
   * allows efficient prefix matching for regular expressions.
   * We could make this even more efficient by sharing pages
   * between views on the same stream.
   */
  private class StreamCharSequence(s: Stream[Char], private var page: String) extends CharSequence {
    lazy val length = s.length
    
    def this(s: Stream[Char]) = this(s, "")
    
    def charAt(index: Int) = {
      while (index > page.length - 1)
        paginate()
      
      page(index)
    }
    
    def subSequence(start: Int, end: Int) = {
      import Math.{min, max}
      
      val newPage = page.subSequence(min(start, page.length - 1), max(page.length - 1, end))
      new StreamCharSequence(s.take(end) drop start, newPage toString)
    }
    
    override def toString = s.mkString
    
    private def paginate() {
      val length = if (page.length == 0) 10 else page.length * 2
      page = s take length mkString
    }
  }
}
