package config

import scala.io.Source
import edu.uwm.cs.gll._

/**
 * Parses config files according to the Git format (ish).  Unlike
 * Git, nested config sections are allowed.  This feature necessarily
 * leads to ambiguity in the parse, which is handled by merging all
 * possible results into a single map.  This illustrates a practical
 * advantage to ambiguous parsing.
 */
object ConfigParser extends common.Example[Map[String, String]] with RegexParsers {
  
  class ConfigException(failures: List[Failure]) extends RuntimeException("Failed to parse config file")
  
  def read(str: String): Map[String, String] = read(LineStream(str))
  
  def read(src: Source): Map[String, String] = read(LineStream(src))
  
  def read(ls: LineStream): Map[String, String] = {
    val results = parser(ls)
    val successful = !results.exists {     // try to find Failure
      case Success(_, _) => false
      case Failure(_, _) => true
    }
    
    if (successful) {
      val back = results.foldLeft(Map[String, String]()) {
        case (map1, Success(map2, _)) => map1 ++ map2
        case (map1, Failure(_, _)) => throw new AssertionError
      }
      
      back
    } else {
      val sorted = results sort { _.tail.length < _.tail.length }
      val length = sorted.head.tail.length
      
      throw new ConfigException(sorted takeWhile { _.tail.length == length } flatMap {
        case s: Success[_] => None
        case f: Failure => Some(f)
      })
    }
  }
  
  def handleSuccesses(forest: List[Map[String, String]]) {
    for ((key, value) <- forest.foldLeft(Map[String, String]()) { _ ++ _ }) {
      println("  " + key + " -> " + value)
    }
  }
  
  // %%
  
  override val whitespace = """[ \t]+"""r  // process newlines separately
  
  lazy val parser = config <~ newline.*     // allow trailing whitespace
  
  private lazy val config = (pairs <~ newline).? ~ sections.? ^^ {
    case (Some(map), maps) => map ++ maps.getOrElse(Map())
    case (None, maps) => maps getOrElse Map()
  }
  
  private lazy val sections: Parser[Map[String, String]] = (
      sections ~ newline.+ ~ section  ^^ { (map1, _, map2) => map1 ++ map2 }
    | section
  )
  
  private lazy val section = ("[" ~> id <~ "]") ~ newline ~ config ^^ { (id, _, map) =>
    val back = for ((key, value) <- map) yield (id + '.' + key) -> value
    back.foldLeft(Map[String, String]()) { _ + _ }
  }
  
  private lazy val pairs: Parser[Map[String, String]] = (
      pairs ~ newline ~ pair    ^^ { (map, _, tuple) => map + tuple }
    | pair                      ^^ { Map(_) }
  )
  
  private lazy val pair = id ~ "=" ~ data    ^^ { (key, _, value) => key -> value }
  
  private val id = """[^\s\[\]=]+"""r
  
  private val data = """([^\n\r\\]|\\.)*"""r
  
  private val newline = """(\n\r|\r\n|\n|\r)"""r    // handles the pain of cross-platform line breaks
}
