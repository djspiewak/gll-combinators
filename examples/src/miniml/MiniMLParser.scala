/*
 * Copyright (c) 2019, Daniel Spiewak
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

package miniml

import com.codecommit.gll._

object MiniMLParser extends common.Example[Any] with RegexParsers {
  
  override val whitespace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r
  
  // %%
  
  lazy val decs = dec*
  
  lazy val dec = (
      "val" ~ pat ~ "=" ~ exp
    | "val" ~ "rec" ~ x ~ "=" ~ "fn" ~ matcher
  ) ^^^ null
  
  lazy val exp: Parser[Any] = (
      x
    | b
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp
    | n
    | exp ~ op ~ exp
    
    | "(" ~ ")"
    | "(" ~ commaExps ~ ")"
    | "fn" ~ matcher
    | exp ~ exp
    
    | "nil"
    | exp ~ "::" ~ exp
    | "case" ~ exp ~ "of" ~ matcher
    | "let" ~ decs ~ "in" ~ exp ~ "end"
  ) ^^^ null
  
  lazy val commaExps: Parser[Any] = (
      exp
    | commaExps ~ "," ~ exp
  )
  
  lazy val matcher: Parser[Any] = (
      mrule
    | mrule ~ "|" ~ matcher
  ) ^^^ null
  
  lazy val mrule = pat ~ "=>" ~ exp ^^^ null
          
  lazy val pat: Parser[Any] = (
      "_"
    | x
    | b
    | n
    | "(" ~ ")"
    | "(" ~ commaPats ~ ")"
    | "nil"
    | pat ~ "::" ~ pat
  ) ^^^ null
  
  lazy val commaPats: Parser[Any] = (
      pat
    | commaPats ~ "," ~ pat
  ) ^^^ null
  
  val x = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  
  val b = "true" | "false"
  
  val n = """~?\d+"""r
  
  val op = (
      "=" | "<" | ">" | "<=" | ">=" 
    | "+" | "-" | "*" | "div" | "mod" 
    | "@" | "o" | "andalso" | "orelse"
  )
  
  // %%
  
  def parser = decs
  
  def handleSuccesses(v: Stream[Any]) {
    if (!v.isEmpty)
      println("  Successfully recognized!")
  }
}
