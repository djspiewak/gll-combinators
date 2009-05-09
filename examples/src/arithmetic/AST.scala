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
