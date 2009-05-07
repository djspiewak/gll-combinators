import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object DisjunctionSpecs extends Specification with ImplicitConversions with ScalaCheck {
  import Prop._
  
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
        p.gather mustEqual List(literal(left), literal(right))
      }
      
      prop must pass
    }
    
    "compute FIRST for binary alternatives" in {
      val prop = forAll { (left: String, right: String) =>
        val leftFirst = if (left.length == 0) Set[Char]() else Set(left charAt 0)
        val rightFirst = if (right.length == 0) Set[Char]() else Set(right charAt 0)
        
        (left | right).first mustEqual (leftFirst ++ rightFirst)
      }
      
      prop must pass
    }
    
    "parse binary alternatives" in {
      {
        val p = "daniel" | "chris"
        
        p("daniel") must beLike {
          case Success("daniel", LineStream()) :: Nil => true
          case _ => false
        }
        
        p("chris") must beLike {
          case Success("chris", LineStream()) :: Nil => true
          case _ => false
        }
      }
      
      {
        val p = "" | ""
        
        p("") must beLike {
          case Success("", LineStream()) :: Nil => true
          case _ => false
        }
      }
    }
    
    "detect PREDCIT failure for LL(1)" in {
      val p = "daniel" | "chris"
      
      p(LineStream('j')) must beLike {
        case Failure("Unexpected value in stream: 'j'", LineStream('j')) :: Nil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure("Unexpected end of stream", LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "detect PREDICT failure for non-LL(1)" in {
      val p = "daniel" | "danielle"
      
      p(LineStream('j')) must beLike {
        case Failure("Unexpected value in stream: 'j'", LineStream('j')) :: Nil => true
        case _ => false
      }
      
      p(LineStream()) must beLike {
        case Failure("Unexpected end of stream", LineStream()) :: Nil => true
        case _ => false
      }
    }
    
    "produce binary failures for LL(1)" in {
      val p = "daniel" | "chris"
      
      p("dan") must beLike {
        case Failure("Unexpected end of stream (expected 'daniel')", LineStream('d', 'a', 'n')) :: Nil => true
        case _ => false
      }
      
      p("dancin") must beLike {
        case Failure("Expected 'daniel' got 'dancin'", LineStream('d', 'a', 'n', 'c', 'i', 'n')) :: Nil => true
        case _ => false
      }
    }
    
    "canonicalize failure message" in {
      val p = literal("") | literal("")
      
      p("\n") must beLike {
        case Failure("Unexpected trailing characters: '\\n'", LineStream('\n')) :: Nil => true
        case _ => false
      }
    }
    
    "produce binary failures for non-LL(1)" in {
      val p = "foobar" | "foobaz"
      
      {
        val data = LineStream("foo")
        
        p(data) must haveTheSameElementsAs(List(
          Failure("Unexpected end of stream (expected 'foobar')", data),
          Failure("Unexpected end of stream (expected 'foobaz')", data)))
      }
      
      {
        val data = LineStream("foobat")
        
        p(data) must haveTheSameElementsAs(List(
          Failure("Expected 'foobar' got 'foobat'", data),
          Failure("Expected 'foobaz' got 'foobat'", data)))
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
      ("daniel" | "chris" | "joseph").first mustEqual Set('d', 'c', 'j')
      ("daniel" | "daniel" | "chris" | "joseph").first mustEqual Set('d', 'c', 'j')
      ("" | "chris" | "" | "daniel" | "daniel").first mustEqual Set()
    }
    
    "parse nary alternatives" in {
      // assumes unambiguous data
      def check(p: Parser[Any], data: String*) = {
        for (str <- data) {
          p(str) must beLike {
            case Success(`str`, LineStream()) :: Nil => true
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
        val pattern = "Expected '%s' got '%s'"
        
        for (str <- data) {
          val stream = LineStream(str)
          val res = p(stream)
          
          val failures = expect.foldRight(List[String]()) { _ :: _ } map { ex => 
            Failure(pattern.format(ex, str), stream)
          }
          
          res must haveTheSameElementsAs(failures)
        }
      }
      
      def check2(p: Parser[Any], expect: String*)(data: String*) = {
        val pattern = "Unexpected end of stream (expected '%s')"
        
        for (str <- data) {
          val stream = LineStream(str)
          val res = p(stream)
          
          val failures = expect.foldRight(List[String]()) { _ :: _ } map { ex => 
            Failure(pattern.format(ex), stream)
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
          
          p(left) must beLike {
            case Success(v, LineStream()) :: Nil => v == f(left)
            case _ => false
          }
          
          p(right) must beLike {
            case Success(v, LineStream()) :: Nil => v == f(right)
            case _ => false
          }
        }
      }
      
      prop must pass
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
        
        result.length mustBe 2
        
        {
          val v = head + suffix
          
          result must have {
            case Success(`v`, LineStream()) => true
            case _ => false
          }
        }
        
        {
          val v = head + " " + suffix
          
          result must have {
            case Success(`v`, LineStream()) => true
            case _ => false
          }
        }
      }
      
      prop must pass
    }
    
    "compute FIRST for left-recursive grammar" in {
      def p: Parser[Any] = p ~ "a" | "a"
      
      p.first mustEqual Set('a')
    }
  }
}
