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
