package lox

import lox.TokenType.*

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Interpreter {
  val environment: Environment = Environment()
  def interpret(statements: ArrayBuffer[Stmt]): Unit = {
    try
      for statement <- statements do
          execute(statement)
    catch
      case error: RuntimeError => Lox.runtimeError(error)
  }

  // Statement Execution
  def execute(stmt: Stmt): Unit = {
    stmt match
      case stmt: Expression => visitExpressionStmt(stmt)
      case stmt: Print => visitPrintStmt(stmt)
      case stmt: Var => visitVarStmt(stmt)
  }
  def visitExpressionStmt(stmt: Expression): Unit = {
    evaluate(stmt.expression)
  }
  def visitPrintStmt(stmt: Print): Unit = {
    val value: Any = evaluate(stmt.expression)
    println(stringify(value))
  }
  def visitVarStmt(stmt: Var): Unit = {
    val value: Any = if stmt.initializer != null then evaluate(stmt.initializer) else null
    environment.define(stmt.name.lexeme, value)
  }

  // Expression Evaluation
  @tailrec
  final def evaluate(expr: Expr): Any = {
    expr match
      case Binary(l, o, r) => visitBinaryExpr(Binary(l, o, r))
      case Grouping(e) => evaluate(e)
      case Literal(v) => v
      case Unary(o, r) => visitUnary(Unary(o, r))
      case Ternary(l, o1, m, o2, r) => visitTernaryExpr(Ternary(l, o1, m, o2, r))
      case expr: Variable => visitVariableExpr(expr)
  }
  private def visitTernaryExpr(expr: Ternary): Any = {
    val left: Any = evaluate(expr.left)
    val middle: Any = evaluate(expr.middle)
    val right: Any = evaluate(expr.right)

    (expr.operator1.tokenType, expr.operator2.tokenType) match
      case (QUESTION_MARK, COLON) => if isTruthy(left) then middle else right
      case _ => assert(false, "Unknown Ternary Operator")
  }
  private def visitBinaryExpr(expr: Binary): Any = {
    val left: Any = evaluate(expr.left)
    val right: Any = evaluate(expr.right)

    expr.operator.tokenType match
      case COMMA => right // already evaluated left, return right
      case BANG_EQUAL => !isEqual(left, right)
      case EQUAL_EQUAL => isEqual(left, right)
      case GREATER =>
        (left, right) match
          case (l: Double, r: Double) => l > r
          case (l: String, r: String) => l > r
          case _ => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case GREATER_EQUAL =>
        (left, right) match
          case (l: Double, r: Double) => l >= r
          case (l: String, r: String) => l >= r
          case _ => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case LESS =>
        (left, right) match
          case (l: Double, r: Double) => l < r
          case (l: String, r: String) => l < r
          case _ => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case LESS_EQUAL =>
        (left, right) match
          case (l: Double, r: Double) => l <= r
          case (l: String, r: String) => l <= r
          case _ => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case MINUS =>
        (left, right) match
          case (l: Double, r: Double) => l - r
          case _ => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case SLASH =>
        (left, right) match
          case (l: Double, r: Double) =>
            if r == 0.0 then throw RuntimeError(expr.operator, "Division by 0.")
            else  l / r
          case _ => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case STAR =>
        (left, right) match
          case (l: Double, r: Double) => l * r
          case _ => throw RuntimeError(expr.operator, "Operands must be numbers.")
      case PLUS =>
        (left, right) match
          case (l: Double, r: Double) => l+r
          case (l: String, r: String) => l+r
          case _ => throw RuntimeError(expr.operator,  "Operands must be two numbers or two strings.")
      case _ => assert(false, "Unknown Binary Operator")
  }
  private def visitUnary(expr: Unary): Any = {
    val right: Any = evaluate(expr.right)
    expr.operator.tokenType match
      case MINUS =>
        right match
          case r: Double => -r
          case _ => RuntimeError(expr.operator, "Operand must be a number.")
      case BANG => !isTruthy(right)
      case _ => assert(false, "Unary Operator Mismatch")
  }

  private def visitVariableExpr(expr: Variable): Any = environment.get(expr.name)

  // Helper Functions
  private def isTruthy(obj: Any): Boolean = {
    obj match
      case null => false
      case bool: Boolean => bool
      case _ => true
  }
  private def isEqual(a: Any, b: Any): Boolean = {
    if a == null && b == null then true
    else if a == null then false
    else a.equals(b)
  }
  private def stringify(obj: Any): String = {
    obj match
      case null => "nil"
      case d: Double =>
        val text: String = d.toString
        if text.endsWith(".0") then text.substring(0, text.length - 2)
        else text
      case o => o.toString
  }







}
