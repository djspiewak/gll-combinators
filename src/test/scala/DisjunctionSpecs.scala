import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object DisjunctionSpecs extends Specification with Parsers with ScalaCheck {
  import Prop._
  import StreamUtils._
  
  "disjunctive parser" should {
    "detect LL(1) grammar" in {
      {
        val p = "daniel" | "chris" | "joseph"
        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustBe true
      }
      
      {
        lazy val p: Parser[String] = (
            "a" ~ p ^^ { _ + _ }
          | "b"
        )
        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustBe true
      }
      
      {
        lazy val p: Parser[String] = (
            "a" ~ p ^^ { _ + _ }
          | "a"
        )
        
        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustBe false
      }
      
      {
        lazy val p: Parser[String] = (
            p ~ "b" ^^ { _ + _ }
          | "a"
        )
        
        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustBe false
      }
      
      {
        lazy val p: Parser[String] = (
            "b" ~ p ^^ { _ + _ }
          | ""
        )
        
        p.asInstanceOf[DisjunctiveParser[String]].isLL1 mustBe false
      }
    }
    
    "gather binary alternatives" in {
      val prop = forAll { (left: String, right: String) =>
        val p = (left | right).asInstanceOf[DisjunctiveParser[String]]
        p.gather == List(literal(left), literal(right))
      }
      
      prop must pass
    }
    
    "compute FIRST for binary alternatives" in {
      import edu.uwm.cs.util._
      
      val prop = forAll { (left: String, right: String) =>
        val leftFirst = if (left.length == 0) Set[Char]() else Set(left charAt 0)
        val rightFirst = if (right.length == 0) Set[Char]() else Set(right charAt 0)
        
        if (leftFirst.size == 0 || rightFirst.size == 0)
          (left | right).first == UniversalCharSet
        else
          (left | right).first == (leftFirst ++ rightFirst)
      }
      
      prop must pass
    }
    
    "parse binary alternatives" in {
      {
        val p = "daniel" | "chris"
        
        p("daniel") must beLike {
          case Success("daniel", LineStream()) #:: SNil => true
          case _ => false
        }
        
        p("chris") must beLike {
          case Success("chris", LineStream()) #:: SNil => true
          case _ => false
        }
      }
      
      {
        val p = "" | ""
        
        p("") must beLike {
          case Success("", LineStream()) #:: SNil => true
          case _ => false
        }
      }
    }
    
    "detect PREDCIT failure for LL(1)" in {
      val p = "daniel" | "chris"
      
      p(LineStream('j')) must beLike {
        case Failure(UnexpectedChars("j"), LineStream('j')) #:: SNil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(None), LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "detect PREDICT failure for non-LL(1)" in {
      val p = "daniel" | "danielle"
      
      p(LineStream('j')) must beLike {
        case Failure(UnexpectedChars("j"), LineStream('j')) #:: SNil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure(UnexpectedEndOfStream(None), LineStream()) #:: SNil => true
        case _ => false
      }
    }
    
    "produce binary failures for LL(1)" in {
      val p = "daniel" | "chris"
      
      p("dan") must beLike {
        case Failure(UnexpectedEndOfStream(Some("daniel")), LineStream('d', 'a', 'n')) #:: SNil => true
        case _ => false
      }
      
      p("dancin") must beLike {
        case Failure(ExpectedLiteral("daniel", "dancin"), LineStream('d', 'a', 'n', 'c', 'i', 'n')) #:: SNil => true
        case _ => false
      }
    }
    
    "canonicalize failure message" in {
      val p = literal("") | literal("")
      
      p("\n") must beLike {
        case Failure(UnexpectedTrailingChars("\\n"), LineStream('\n')) #:: SNil => true
        case _ => false
      }
    }
    
    "produce binary failures for non-LL(1)" in {
      val p = "foobar" | "foobaz"
      
      {
        val data = LineStream("foo")
        
        p(data) must haveTheSameElementsAs(List(
          Failure(UnexpectedEndOfStream(Some("foobar")), data),
          Failure(UnexpectedEndOfStream(Some("foobaz")), data)))
      }
      
      {
        val data = LineStream("foobat")
        
        p(data) must haveTheSameElementsAs(List(
          Failure(ExpectedLiteral("foobar", "foobat"), data),
          Failure(ExpectedLiteral("foobaz", "foobat"), data)))
      }
    }
    
    "gather nary alternatives" in {
      def check(p: Parser[Any], expected: Parser[String]*) {
        p.asInstanceOf[DisjunctiveParser[String]].gather must containAll(expected)
      }
      
      {
        val p = "daniel" | "chris" | "joseph"
        check(p, "daniel", "chris", "joseph")
      }
      
      {
        val p = "daniel" | "daniel" | "chris" | "joseph"
        check(p, "daniel", "chris", "joseph")
      }
      
      {
        val p = "" | "chris" | "" | "daniel" | "daniel"
        check(p, "", "daniel", "chris")
      }
    }
    
    "compute FIRST for nary alternatives" in {
      import edu.uwm.cs.util._
      
      ("daniel" | "chris" | "joseph").first mustEqual Set('d', 'c', 'j')
      ("daniel" | "daniel" | "chris" | "joseph").first mustEqual Set('d', 'c', 'j')
      ("" | "chris" | "" | "daniel" | "daniel").first mustEqual UniversalCharSet
    }
    
    "parse nary alternatives" in {
      // assumes unambiguous data
      def check(p: Parser[Any], data: String*) = {
        for (str <- data) {
          p(str) must beLike {
            case Success(`str`, LineStream()) #:: SNil => true
            case _ => false
          }
        }
      }
      
      {
        val p = "daniel" | "chris" | "joseph" | "renee" | "bethany" | "grace"
        check(p, "daniel", "chris", "joseph", "renee", "bethany", "grace")
      }
      
      {
        val p = "renee" | "bethany" | "grace"
        check(p, "renee", "bethany", "grace")
      }
      
      {
        val p = "daniel" | "chris" | "joseph" | "renee"
        check(p, "daniel", "chris", "joseph", "renee")
      }
    }
    
    "produce nary failures for LL(1)" in {
      // assumes unambiguous data
      def check1(p: Parser[Any], expect: String*)(data: String*) = {
        for (str <- data) {
          val stream = LineStream(str)
          val res = p(stream)
          
          val failures = expect.foldRight(List[String]()) { _ :: _ } map { ex => 
            Failure(ExpectedLiteral(ex, str), stream)
          }
          
          res must haveTheSameElementsAs(failures)
        }
      }
      
      def check2(p: Parser[Any], expect: String*)(data: String*) = {
        for (str <- data) {
          val stream = LineStream(str)
          val res = p(stream)
          
          val failures = expect.foldRight(List[String]()) { _ :: _ } map { ex => 
            Failure(UnexpectedEndOfStream(Some(ex)), stream)
          }
          
          res must haveTheSameElementsAs(failures)
        }
      }
      
      {
        val p = "daniel" | "chris" | "joseph" | "renee" | "bethany" | "grace"
        
        check1(p, "daniel")("dancin")
        check1(p, "chris")("chari")
        check1(p, "joseph")("josefs")
        
        check2(p, "daniel")("dan", "d")
        check2(p, "joseph")("joe", "j")
        check2(p, "bethany")("beth", "b")
      }
      
      {
        val p = "renee" | "bethany" | "grace"
        
        check1(p, "renee")("rainb")
        check1(p, "bethany")("battiny")
        check1(p, "grace")("grabo")
        
        check2(p, "renee")("ren", "r")
        check2(p, "bethany")("beth", "b")
        check2(p, "grace")("gr", "g")
      }
    }
    
    "map results" in {
      val prop = forAll { (left: String, right: String, f: String=>Int) =>
        left != right ==> {
          val p = (
              left
            | right
          ) ^^ f
          
          val res1 = p(left) match {
            case Success(v, LineStream()) #:: SNil => v == f(left)
            case _ => false
          }
          
          val res2 = p(right) match {
            case Success(v, LineStream()) #:: SNil => v == f(right)
            case _ => false
          }
          
          res1 && res2
        }
      }
      
      prop must pass
    }
    
    "map results with stream tail" in {
      var in1: LineStream = null
      val p1 = ("foo" | "bar") ^# { (in, s) =>
        in1 = in
        s
      }
      
      var in2: LineStream = null
      val p2 = ("baz" | "bin") ^# { (in, s) =>
        in2 = in
        s
      }
      
      val p = p1 ~ "\n" ~> p2
      
      p("foo\nbaz") must beLike {
        case Success("baz", LineStream()) #:: SNil => true
        case _ => false
      }
      
      in1 mustNot beNull
      in1.line mustEqual "foo"
      in1.lineNum mustEqual 1
      in1.head mustBe 'f'
      in1.toString mustEqual "foo\nbaz"
      
      in2 mustNot beNull
      in2.line mustEqual "baz"
      in2.lineNum mustEqual 2
      in2.head mustBe 'b'
      in2.toString mustEqual "baz"
      
      
      p("foo\nbin") must beLike {
        case Success("bin", LineStream()) #:: SNil => true
        case _ => false
      }
      
      in1 mustNot beNull
      in1.line mustEqual "foo"
      in1.lineNum mustEqual 1
      in1.head mustBe 'f'
      in1.toString mustEqual "foo\nbin"
      
      in2 mustNot beNull
      in2.line mustEqual "bin"
      in2.lineNum mustEqual 2
      in2.head mustBe 'b'
      in2.toString mustEqual "bin"
      
      p("bar\nbaz") must beLike {
        case Success("baz", LineStream()) #:: SNil => true
        case _ => false
      }
      
      in1 mustNot beNull
      in1.line mustEqual "bar"
      in1.lineNum mustEqual 1
      in1.head mustBe 'b'
      in1.toString mustEqual "bar\nbaz"
      
      in2 mustNot beNull
      in2.line mustEqual "baz"
      in2.lineNum mustEqual 2
      in2.head mustBe 'b'
      in2.toString mustEqual "baz"
      
      p("bar\nbin") must beLike {
        case Success("bin", LineStream()) #:: SNil => true
        case _ => false
      }
      
      in1 mustNot beNull
      in1.line mustEqual "bar"
      in1.lineNum mustEqual 1
      in1.head mustBe 'b'
      in1.toString mustEqual "bar\nbin"
      
      in2 mustNot beNull
      in2.line mustEqual "bin"
      in2.lineNum mustEqual 2
      in2.head mustBe 'b'
      in2.toString mustEqual "bin"
    }
    
    "handle binary shift/reduce ambiguity" in {
      val prop = forAll { (head: String, suffix: String) =>
        // %%
        
        val p1 = (
            head ~ suffix     ^^ { _ + _ }
          | head
        )
        
        val p2 = "" | suffix  ^^ { " " + _ }
        
        val p = p1 ~ p2       ^^ { _ + _ }
        
        // %%
        
        val result = p(head + suffix)
        
        result.length mustEqual 2
       
        result mustContain Success(head + suffix, LineStream())
        result mustContain Success(head + " " + suffix, LineStream())
      }
      
      prop must pass
    }
    
    "compute FIRST for left-recursive grammar" in {
      lazy val p: Parser[Any] = p ~ "a" | "a"
      
      p.first mustEqual Set('a')
    }
  }
}
