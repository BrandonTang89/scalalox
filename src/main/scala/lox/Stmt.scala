package lox
trait Stmt
case class Expression(expression: Expr) extends Stmt
case class Print(expression: Expr) extends Stmt
case class Var(name: Token, initializer: Expr) extends Stmt
