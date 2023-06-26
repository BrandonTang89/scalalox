package lox

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.boundary, boundary.break
class Resolver(val interpreter: Interpreter) {

  private val scopes: mutable.Stack[mutable.HashMap[String, Boolean]] = mutable.Stack[mutable.HashMap[String, Boolean]]()
  def resolve(statements: ArrayBuffer[Stmt]): Unit = statements.foreach(resolve)
  private def resolve(stmt: Stmt): Unit = {
    stmt match
      case stmt: Expression => visitExpressionStmt(stmt)
      case stmt: Print => visitPrintStmt(stmt)
      case stmt: Var => visitVarStmt(stmt)
      case stmt: Block => visitBlockStmt(stmt)
      case stmt: If => visitIfStmt(stmt)
      case stmt: While => visitWhileStmt(stmt)
      case stmt: Break => // nothing to do
      case stmt: Continue => // nothing to do
      case stmt: Return => visitReturnStmt(stmt)
      case stmt: FunctionDec => visitFunctionDeclStmt(stmt)
  }
  @tailrec
  private def resolve(expr: Expr): Unit =
    expr match
      case expr: Binary => visitBinaryExpr(expr)
      case Grouping(e) => resolve(e)
      case Literal(v) => // nothing to do
      case expr: Unary => visitUnary(expr)
      case expr: Ternary => visitTernaryExpr(expr)
      case expr: Variable => visitVariableExpr(expr)
      case expr: Assign => visitAssignExpr(expr)
      case expr: Logical => visitLogicalExpr(expr)
      case expr: Call => visitCallExpr(expr)
      case expr: Lambda => visitLambdaExpr(expr)

  // Interesting Visits
  private def beginScope(): Unit =
    scopes.push(new mutable.HashMap[String, Boolean]())
  private def endScope(): Unit =
    scopes.pop()
  private def declare(name: Token): Unit =
    if scopes.nonEmpty then
      if scopes.top.contains(name.lexeme) then Lox.error(name, "Already a variable with this name in this scope.")
      scopes.top.put(name.lexeme, false)
  private def define(name: Token): Unit =
    if scopes.nonEmpty then scopes.top.put(name.lexeme, true)
  private def resolveLocal(expr: Expr, name: Token): Unit = // expr could be a variable or assignment
    boundary: // Scala 3 mechanism
      for i <- scopes.indices do
        if scopes(i).contains(name.lexeme) then
          interpreter.resolve(expr, i)
          break()
  private def resolveFunction(lambda: Lambda): Unit =
    beginScope()
    lambda.parameters.foreach(p =>
      declare(p)
      define(p)
    )
    resolve(lambda.body)
    endScope()

  private def visitBlockStmt(stmt: Block): Unit =
    beginScope()
    resolve(stmt.statements)
    endScope()
  private def visitVarStmt(stmt: Var): Unit = // variable declaration
    declare(stmt.name)
    if stmt.initializer.isDefined then
      stmt.initializer match
        case None => define(stmt.name)
        case Some(lambda: Lambda) => // only allow self reference for functions
          define(stmt.name)
          resolve(lambda)
        case Some(expr) =>
          resolve(expr)
          define(stmt.name)

  private def visitAssignExpr(expr: Assign): Unit =
    resolve(expr.value)
    resolveLocal(expr, expr.name)
  private def visitVariableExpr(expr: Variable): Unit =
    if scopes.nonEmpty && scopes.top.get(expr.name.lexeme).contains(false) then
      Lox.error(expr.name, "Can't read local variable in its own initializer.")
    resolveLocal(expr, expr.name)
  private def visitFunctionDeclStmt(stmt: FunctionDec): Unit =
    declare(stmt.name)
    define(stmt.name)
    resolveFunction(Lambda(stmt.name, stmt.params, stmt.body))
  private def visitLambdaExpr(lambda: Lambda): Unit =
    resolveFunction(lambda)

  // Uninteresting Visits
  private def visitExpressionStmt(stmt: Expression): Unit = resolve(stmt.expression)
  private def visitIfStmt(stmt: If): Unit =
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    if stmt.elseBranch != null then resolve(stmt.elseBranch)

  private def visitPrintStmt(stmt: Print): Unit = resolve(stmt.expression)
  private def visitReturnStmt(stmt: Return): Unit =
    if stmt.value != null then resolve(stmt.value)

  private def visitWhileStmt(stmt: While): Unit =
    resolve(stmt.condition)
    resolve(stmt.body)

  private def visitBinaryExpr(expr: Binary): Unit =
    resolve(expr.left)
    resolve(expr.right)

  private def visitTernaryExpr(expr: Ternary): Unit =
    resolve(expr.left)
    resolve(expr.middle)
    resolve(expr.right)

  private def visitCallExpr(expr: Call): Unit =
    resolve(expr.calle)
    expr.arguments.foreach(resolve)

  private def visitLogicalExpr(expr: Logical): Unit =
    resolve(expr.left)
    resolve(expr.right)

  private def visitUnary(expr: Unary): Unit = resolve(expr.right)
}
