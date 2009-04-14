import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object ParserSpecs extends Specification with ScalaCheck with ImplicitConversions {
  import Prop._
  
  "literal parser" should {
    "parse single tokens" in {
      val p = literal("test")
      
      p("test" toStream) must beLike {
        case Success("test", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "parse the empty string" in {
      val p = literal("")
      
      p("test" toStream) must beLike {
        case Success("", Stream('t', 'e', 's', 't')) :: Nil => true     // should I keep this??
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      val prop = forAll { s: String =>
        if (s.length == 0)
          literal(s).first == Set()
        else
          literal(s).first == Set(s charAt 0)
      }
      
      prop must pass
    }
    
    "map results according to a function" in {
      val p = "test" ^^ { _.length }
      
      p("test" toStream) match {
        case Success(4, Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "map results according to a value" in {
      val p = "test" ^^^ 42
      
      p("test" toStream) match {
        case Success(42, Stream()) :: Nil => true
        case _ => false
      }
    }
  }
  
  "terminal sequential parser" should {
    "parse multiple tokens" in {
      val p = "te" ~ "st"
      
      p("test" toStream) must beLike {
        case Success("te" ~ "st", Stream()) :: Nil => true
        case _ => false
      }
    }
    
    "compute FIRST set" in {
      val prop = forAll { strs: List[String] =>
        strs.length > 0 ==> {
          val p = strs.map(literal).reduceLeft[Parser[Any]] { _ ~ _ }
          val first = strs.foldLeft(Set[Char]()) { (set, str) =>
            if (set.size == 0 && str.length > 0) Set(str charAt 0) else set
          }
          
          p.first mustEqual first
        }
      }
      
      prop must pass
    }
  }
  
  "disjunctive parser" should {
    "gather binary alternatives" in {
      val prop = forAll { (left: String, right: String) =>
        (left | right).asInstanceOf[DisjunctiveParser[String]].gather == Set(literal(left), literal(right))
      }
      
      prop must pass
    }
    
    "compute FIRST for binary alternatives" in {
      val prop = forAll { (left: String, right: String) =>
        val leftFirst = if (left.length == 0) Set[Char]() else Set(left charAt 0)
        val rightFirst = if (right.length == 0) Set[Char]() else Set(right charAt 0)
        
        (left | right).first == leftFirst ++ rightFirst
      }
      
      ("" | "").first mustEqual Set()
      
      prop must pass
    }
    
    "parse binary alternatives" in {
      {
        val p = "daniel" | "chris"
        
        p("daniel" toStream) must beLike {
          case Success("daniel", Stream()) :: Nil => true
          case _ => false
        }
        
        p("chris" toStream) must beLike {
          case Success("chris", Stream()) :: Nil => true
          case _ => false
        }
      }
      
      {
        val p = "" | ""
        
        p("" toStream) must beLike {
          case Success("", Stream()) :: Nil => true
          case _ => false
        }
      }
    }
    
    "gather nary alternatives" in {
      val prop = forAll { strs: List[String] =>
        strs.length > 1 ==> {
          val parsers = strs.map(literal)
          val p = parsers.reduceLeft[Parser[Any]] { _ | _ }
          val potentials = Set(parsers:_*)
          
          p.asInstanceOf[DisjunctiveParser[Any]].gather == potentials
        }
      }
      
      prop must pass
    }
    
    "compute FIRST for nary alternatives" in {
      val prop = forAll { strs: List[String] =>
        strs.length > 1 ==> {
          val p = strs.map(literal).reduceLeft[Parser[Any]] { _ | _ }
          val first = strs.foldLeft(Set[Char]()) { (set, str) =>
            if (str.length == 0) set else set + str.charAt(0)
          }
          
          p.first == first
        }
      }
      
      prop must pass
    }
    
    "parse nary alternatives" in {
      // assumes unambiguous data
      def check(data: String*) = {
        val p = data.map(literal).reduceLeft[Parser[Any]] { _ | _ }
        
        for (str <- data) {
          p(str toStream) must beLike {
            case Success(str, Stream()) :: Nil => true
            case _ => false
          }
        }
      }
      
      check("daniel", "chris", "joseph", "renee", "bethany", "grace")
      check("renee", "bethany", "grace")
      check("daniel", "chris", "joseph", "renee")
    }
    
    "map results" in {
      val prop = forAll { (left: String, right: String, f: String=>Int) =>
        left != right ==> {
          val p = (
              left
            | right
          ) ^^ f
          
          (p(left toStream) match {
            case Success(v, Stream()) :: Nil => v == f(left)
            case _ => false
          }) && (p(right toStream) match {
            case Success(v, Stream()) :: Nil => v == f(right)
            case _ => false
          })
        }
      }
      
      prop must pass
    }
    
    "handle binary shift/reduce ambiguity" in {
      val prop = forAll { (head: String, suffix: String) =>
        val p = head ~ suffix | (head + suffix)
        val result = p((head + suffix) toStream)
        
        val a = {
          result exists {
            case Success(`head` ~ `suffix`, Stream()) => true
            case _ => false
          }
        }
        
        val b = {
          val v = head + suffix
          
          result exists {
            case Success(`v`, Stream()) => true
            case x => false
          }
        }
        
        a && b
      }
      
      val p = "" | ""
      p("" toStream) mustHave {
        case Success("", Stream()) => true
        case _ => false
      }
      
      prop must pass
    }
  }
  
  "compound non-terminal parsers" should {
    "parse an unambiguous right-recursive grammar" in {
      def p: Parser[Any] = (
          "a" ~ p
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data toStream) must beLike {
          case Success(`data`, Stream()) :: Nil => true   // TODO
          case _ => false
        }
      }
      
      p mustNot throwA[Throwable]
      
      check("a")
      check("aaaaaaaa")
      check("aaaaa")
      check((0 to 100000).foldLeft("a") { (s, _) => s + "a" })      // *really* long input
      check("aa")
    }
    
    /* "parse an unambiguous left-recursive grammar" in {
      def p: Parser[Any] = (
          p ~ "a"
        | "a"
      )
      
      // assumes data =~ /a+/
      def check(data: String) {
        p(data toStream) must beLike {
          case Success(`data`, Stream()) => true
          case _ => false
        }
      }
      
      p mustNot throwA[Throwable]
      
      check("a")
      check("aaaaaaaa")
      check("aaaaa")
      check((0 to 100000).foldLeft("a") { (s, _) => s + "a" })      // *really* long input
      check("aa")
    } */
  }
}
