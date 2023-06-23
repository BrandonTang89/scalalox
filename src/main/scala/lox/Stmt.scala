package lox

import scala.collection.mutable.ArrayBuffer

trait Stmt
case class Expression(expression: Expr) extends Stmt
case class Print(expression: Expr) extends Stmt
case class Var(name: Token, initializer: Option[Expr]) extends Stmt
case class Block(statements: ArrayBuffer[Stmt]) extends Stmt
case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt
case class While(condition: Expr, body: Stmt) extends Stmt
case class Break() extends Stmt
case class Continue() extends Stmt