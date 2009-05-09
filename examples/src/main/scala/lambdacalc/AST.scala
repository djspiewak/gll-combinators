package lambdacalc

object AST {
  type Env = Map[Symbol, Lambda]
  
  case class Alias(id: Symbol, expr: Expr)
  
  sealed trait Expr {
    def eval(env: Env): Lambda
    
    def format(tab: String): String
    
    override def toString = format("")
  }
  
  case class Lambda(id: Symbol, expr: Expr) extends Expr {
    def eval(env: Env) = this   // no beta evaluation
    
    def apply(env: Env, arg: Lambda) = expr.eval(env(id) = arg)
    
    def format(tab: String) = {
      val Symbol(arg) = id
      "%s(\u03BB%s .%n%s)".format(tab, arg, expr.format(tab + "  "))
    }
  }
  
  case class App(e1: Expr, e2: Expr) extends Expr {
    def eval(env: Env) = {
      val lambda = e1.eval(env)
      lambda(env, e2.eval(env))
    }
    
    def format(tab: String) = "%s%n%s".format(e1.format(tab), e2.format(tab))
  }
  
  case class Val(id: Symbol) extends Expr {
    def eval(env: Env) = env.get(id) match {
      case Some(lam) => lam
      case None => throw new RuntimeException("Identifier %s' is unbound".format(id))
    }
    
    def format(tab: String) = {
      val Symbol(name) = id
      "%s%s".format(tab, name)
    }
  }
}
