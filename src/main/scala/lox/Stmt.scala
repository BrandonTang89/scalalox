package lox

import scala.collection.mutable.ArrayBuffer

trait Stmt
case class Expression(expression: Expr) extends Stmt
case class Print(expression: Expr) extends Stmt
case class Var(name: Token, initializer: Option[Expr]) extends Stmt
case class Block(statements: ArrayBuffer[Stmt]) extends Stmt
