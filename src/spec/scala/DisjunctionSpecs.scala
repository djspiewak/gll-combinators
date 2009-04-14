import edu.uwm.cs.gll._

import org.specs._
import org.scalacheck._

object DisjunctionSpecs extends Specification with ImplicitConversions with ScalaCheck {
  import Prop._
  
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
    
    "compute FIRST for left-recursive grammar" in {
      def p: Parser[Any] = p | "a"
      
      p.first mustEqual Set('a')
    }
  }
}
