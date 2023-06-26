package lox

import lox.TokenType.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Interpreter{
  private val globals: Environment = Environment()
  private val locals: mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()
  var environment: Environment = globals

  // Native Functions
  globals.define("clock", new LoxCallable{
    def arity: Int = 0
    def call(interpreter: Interpreter, arguments: ArrayBuffer[Any]): Any = {
      System.currentTimeMillis().toDouble / 1000.0
    }
    override def toString: String = "<native clock fn>"
  })

  // Scoping Resolution
  def resolve(expr: Expr, depth: Int): Unit = {
    expr match
      case Variable(n, i) => locals.put(i, depth)
      case Assign(n, v, i) => locals.put(i, depth)
  }

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
      case stmt: Block => visitBlockStmt(stmt)
      case stmt: Break => throw Interpreter.loopBreakException(stmt)
      case stmt: Continue => throw Interpreter.loopContinueException(stmt)
      case stmt: Expression => visitExpressionStmt(stmt)
      // case stmt: FunctionDec => visitFunctionDeclStmt(stmt)
      case stmt: If => visitIfStmt(stmt)
      case stmt: Print => visitPrintStmt(stmt)
      case stmt: Return => visitReturnStmt(stmt)
      case stmt: Var => visitVarStmt(stmt)
      case stmt: While => visitWhileStmt(stmt)
  }
  private def visitBlockStmt(stmt: Block): Unit = {
    executeBlock(stmt.statements, Environment(environment))
  }
  def executeBlock(statements: ArrayBuffer[Stmt], environment: Environment): Unit = {
    val previous: Environment = this.environment
    try
      this.environment = environment
      for statement <- statements do
        execute(statement)
    finally
      this.environment = previous
  }
  private def visitExpressionStmt(stmt: Expression): Unit = {
    evaluate(stmt.expression)
  }

  private def visitReturnStmt(stmt: Return): Unit = {
    var value: Any = null
    if stmt.value != null then value = evaluate(stmt.value)
    throw ReturnException(value)
  }

  /** Deprecated function Declaration (replaced with variable assignment to lambdas)*/
  /*
  private def visitFunctionDeclStmt(stmt: FunctionDec): Unit = {
    val function: LoxFunction = LoxFunction(stmt.name, Lambda(stmt.name, stmt.params, stmt.body), environment)
    environment.define(stmt.name.lexeme, function)
  }
  */

  private def visitWhileStmt(stmt: While): Unit = {
    while isTruthy(evaluate(stmt.condition)) do
      try
        execute(stmt.body)
      catch
        case e: Interpreter.loopBreakException => return
        case e: Interpreter.loopContinueException => // just continue
  }
  private def visitIfStmt(stmt: If): Unit = {
    if isTruthy(evaluate(stmt.condition)) then
      execute(stmt.thenBranch)
    else if stmt.elseBranch != null then execute(stmt.elseBranch)
  }

  private def visitPrintStmt(stmt: Print): Unit = {
    val value: Any = evaluate(stmt.expression)
    println(stringify(value))
  }
  private def visitVarStmt(stmt: Var): Unit = {
    stmt.initializer match
      case None => environment.define(stmt.name.lexeme)
      case Some(expr) => environment.define(stmt.name.lexeme, evaluate(expr))
  }

  // Expression Evaluation
  @tailrec
  final def evaluate(expr: Expr): Any = {
    expr match
      case expr: Assign => visitAssignExpr(expr)
      case expr: Binary => visitBinaryExpr(expr)
      case expr: Call => visitCallExpr(expr)
      case Grouping(e) => evaluate(e)
      case Literal(v) => v
      case expr: Lambda => visitLambdaExpr(expr)
      case expr: Logical => visitLogicalExpr(expr)
      case expr: Ternary => visitTernaryExpr(expr)
      case expr: Variable => visitVariableExpr(expr)
      case expr: Unary => visitUnary(expr)
  }

  private def visitLambdaExpr(expr: Lambda): Any = {
    LoxFunction(expr.keyword, expr, environment)
  }

  /** Returns an object with the same truthiness value as the boolean
   *  expression should evaluate to.
   */
  private def visitLogicalExpr(expr: Logical): Any = {
    val left: Any = evaluate(expr.left)

    expr.operator.tokenType match
      case OR => if isTruthy(left) then left else evaluate(expr.right)
      case AND => if !isTruthy(left) then left else evaluate(expr.right)
      case _ => assert(false, "Unexpected logical operator")
  }
  private def visitAssignExpr(expr: Assign): Any = {
    val value: Any = evaluate(expr.value)

    locals.get(expr.index) match
      case None => globals.assign(expr.name, value)
      case Some(distance) => environment.assignAt(distance, expr.name, value)

    value
  }
  private def visitTernaryExpr(expr: Ternary): Any = {
    val left: Any = evaluate(expr.left)

    (expr.operator1.tokenType, expr.operator2.tokenType) match
      case (QUESTION_MARK, COLON) => if isTruthy(left) then evaluate(expr.middle) else evaluate(expr.right)
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

  private def visitCallExpr(expr: Call): Any = {
    val callee: Any = evaluate(expr.calle)
    val arguments: ArrayBuffer[Any] = expr.arguments.map(evaluate)
    callee match
      case function: LoxCallable =>
        if arguments.size != function.arity then
          throw RuntimeError(expr.paren, "Expected " + function.arity + " arguments but got" +
            arguments.size + ".")

        else function.call(this, arguments)
      case _ => throw RuntimeError(expr.paren, "Can only call functions and classes.")
  }

  private def visitVariableExpr(expr: Variable): Any = lookUpVariable(expr.name, expr)
  private def lookUpVariable(name: Token, expr: Variable): Any = {
    val dist: Option[Int] = locals.get(expr.index)
    dist match
      case None => globals.get(name)
      case Some(d) => environment.getAt(d, name)
  }

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
object Interpreter{
  private case class loopBreakException(breakStatement: Stmt) extends Exception
  private case class loopContinueException(continueStatement: Stmt) extends Exception
}
