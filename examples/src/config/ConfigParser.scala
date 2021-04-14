/*
 * Copyright (c) 2021, Daniel Spiewak
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package config

import scala.collection.compat.immutable.LazyList
import scala.io.Source
import com.codecommit.gll._

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
        case (_, Failure(_, _)) => throw new AssertionError
      }
      
      back
    } else {
      val sorted = results.toList sortWith { _.tail.length < _.tail.length }
      val length = sorted.head.tail.length
      
      throw new ConfigException(sorted takeWhile { _.tail.length == length } flatMap {
        case _: Success[_] => None
        case f: Failure => Some(f)
      })
    }
  }
  
  def handleSuccesses(forest: LazyList[Map[String, String]]): Unit = {
    for ((key, value) <- forest.foldLeft(Map[String, String]()) { _ ++ _ }) {
      println("  " + key + " -> " + value)
    }
  }
  
  // %%
  
  override val whitespace = """[ \t]+"""r  // process newlines separately
  
  lazy val parser = config <~ newlines     // allow trailing whitespace
  
  private lazy val config: Parser[Map[String, String]] = (
      pairs ~ newlines ~ sections ^^ combineMaps
    | pairs                       
    | sections                    
    | ""                          ^^^ Map()
  )
  
  private lazy val sections: Parser[Map[String, String]] = (
      sections ~ newlines ~ section  ^^ combineMaps
    | section                         
  )
  
  private lazy val section = ("[" ~> id <~ "]") ~ newlines ~ config ^^ { (id, _, map) =>
    val back = for ((key, value) <- map) yield (id + '.' + key) -> value
    back.foldLeft(Map[String, String]()) { _ + _ }
  }
  
  private lazy val pairs: Parser[Map[String, String]] = (
      pairs ~ newlines ~ pair ^^ { (map, _, pair) => map + pair }
    | pair                    ^^ { Map(_) }
  )
  
  private lazy val pair = id ~ "=" ~ data ^^ { (key, _, value) => key -> value }
  
  private val id = """[^\s\[\]=]+"""r
  
  private val data = """([^\n\r\\]|\\.)*"""r
  
  private val newlines = """([ \t]*(\n\r|\r\n|\n|\r))+"""r    // handles the pain of cross-platform line breaks
  
  // %%
  
  private def combineMaps(map1: Map[String, String], s: Any, map2: Map[String, String]) = map1 ++ map2
}
