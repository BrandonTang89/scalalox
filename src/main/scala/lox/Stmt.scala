package lox
import scala.collection.mutable.ArrayBuffer

trait Stmt
case class Expression(expression: Expr) extends Stmt
case class Print(expression: Expr) extends Stmt
case class Var(name: Token, initializer: Option[Expr]) extends Stmt
case class Block(statements: ArrayBuffer[Stmt]) extends Stmt
case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt
case class While(condition: Expr, body: Stmt) extends Stmt
case class Break(keyword: Token) extends Stmt
case class Continue(keyword: Token) extends Stmt
case class FunctionDec(name: Token, params: ArrayBuffer[Token], body: ArrayBuffer[Stmt]) extends Stmt
// Not necessary since introduction of Lambda functions.
case class Return(keyword: Token, value: Expr) extends Stmt
case class Class(name: Token, superclass: Option[Variable], methods: ArrayBuffer[Var]) extends Stmt // The Var list is the list of methods