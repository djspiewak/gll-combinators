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

package lambdacalc

object AST {
  type Env = scala.collection.mutable.Map[Symbol, Lambda]
  
  case class Alias(id: Symbol, expr: Expr)
  
  sealed trait Expr {
    def eval(env: Env): Lambda
    
    def sub(id: Symbol, value: Lambda): Expr
  }
  
  case class Lambda(id: Symbol, expr: Expr) extends Expr {
    def eval(env: Env) = this   // no beta evaluation
    
    def apply(env: Env, arg: Lambda) = expr.sub(id, arg).eval(env)
    
    def sub(id: Symbol, value: Lambda) = {
      if (this.id == id)
        this
      else
        Lambda(this.id, expr.sub(id, value))
    }
    
    override def toString = {
      val Symbol(name) = id
      "(\u03BB%s . %s)".format(name, expr)
    }
  }
  
  case class App(e1: Expr, e2: Expr) extends Expr {
    def eval(env: Env) = {
      val lambda = e1.eval(env)
      lambda(env, e2.eval(env))
    }
    
    def sub(id: Symbol, value: Lambda) = App(e1.sub(id, value), e2.sub(id, value))
    
    override def toString = "(%s %s)".format(e1, e2)
  }
  
  case class Val(id: Symbol) extends Expr {
    def eval(env: Env) = env.get(id) match {
      case Some(lam) => lam
      case None => throw EvalException("Identifier %s' is unbound".format(id))
    }
    
    def sub(id: Symbol, value: Lambda) = {
      if (this.id == id)
        value
      else
        this
    }
    
    override def toString = {
      val Symbol(name) = id
      name
    }
  }
  
  case class EvalException(msg: String) extends RuntimeException(msg)
}
