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

package arithmetic

object AST {
  sealed trait Expr {
    val solve: Int
    val complex: Boolean
    
    def format(tab: String): String
    
    override def toString = format("  ")
  }
  
  sealed trait BinExpr {
    val complex = true
    
    val e1: Expr
    val e2: Expr
    
    val sym: Char
    
    def format(tab: String) = {
      var back = tab + "(" + sym
      
      if (e1.complex || e2.complex) {
        back += '\n'
        back += e1.format(tab + "  ")
        
        back += '\n'
        back += e2.format(tab + "  ")
      } else {
        back += e1.format(" ")
        back += e2.format(" ")
      }
      
      back += ")"
      
      back
    }
  }
  
  case class Add(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '+'
    
    val solve = e1.solve + e2.solve
  }
  
  case class Sub(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '-'
    
    val solve = e1.solve - e2.solve
  }
  
  case class Mul(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '*'
    
    val solve = e1.solve * e2.solve
  }
  
  case class Div(e1: Expr, e2: Expr) extends Expr with BinExpr {
    val sym = '/'
    
    val solve = e1.solve / e2.solve
  }
  
  case class Neg(e: Expr) extends Expr {
    val solve = -e.solve
    val complex = e.complex
    
    def format(tab: String) = {
      if (e.complex)
        tab + "(neg\n" + e.format(tab + "  ") + ")"
      else
        tab + "(neg" + e.format(" ") + ")"
    }
  }
  
  case class IntLit(i: Int) extends Expr {
    val solve = i
    val complex = false
    
    def format(tab: String) = tab + i
  }
}
