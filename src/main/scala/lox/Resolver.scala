package lox

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.boundary
import boundary.break
import lox.Resolver.{ClassType, FunctionType, LoopType, localVarStatus}

class Resolver(val interpreter: Interpreter) {
  private val scopes: mutable.Stack[mutable.HashMap[String, (Token, localVarStatus)]] = mutable.Stack[mutable.HashMap[String, (Token, localVarStatus)]]()
  private var currentFunction: FunctionType= FunctionType.NONE
  private var currentClass: ClassType = ClassType.NONE
  private var withinLoop: LoopType = LoopType.NONE
  def resolve(statements: ArrayBuffer[Stmt]): Unit = statements.foreach(resolve)
  private def resolve(stmt: Stmt): Unit = {
    stmt match
      case stmt: Expression => visitExpressionStmt(stmt)
      case stmt: Print => visitPrintStmt(stmt)
      case stmt: Var => visitVarStmt(stmt)
      case stmt: Block => visitBlockStmt(stmt)
      case stmt: If => visitIfStmt(stmt)
      case stmt: While => visitWhileStmt(stmt)
      case stmt: Break => if withinLoop == LoopType.NONE then Lox.error(stmt.keyword, "Unexpected 'break' outside a loop.")
      case stmt: Continue => if withinLoop == LoopType.NONE then Lox.error(stmt.keyword, "Unexpected 'continue' outside a loop.")
      case stmt: Return => visitReturnStmt(stmt)
      case stmt: FunctionDec => visitFunctionDeclStmt(stmt)
      case stmt: Class => visitClassStmt(stmt)
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
      case expr: Get => visitGetExpr(expr)
      case expr: Set => visitSetExpr(expr)
      case expr: This => visitThisExpr(expr)
      case expr: Super => visitSuperExpr(expr)

  // Interesting Visits
  private def beginScope(): Unit =
    scopes.push(new mutable.HashMap[String, (Token, localVarStatus)]())
  private def endScope(): Unit =
    scopes.top.foreach((lexeme, x) => if x._2 != localVarStatus.USED then Lox.error(x._1, "Unused Local Variable."))
    scopes.pop()
  private def declare(name: Token): Unit = // declare a variable in the current scope
    if scopes.nonEmpty then
      if scopes.top.contains(name.lexeme) then Lox.error(name, "Already a variable with this name in this scope.")
      scopes.top.put(name.lexeme, (name, localVarStatus.DECLARED))
  private def define(name: Token): Unit =
    if scopes.nonEmpty then scopes.top.put(name.lexeme, (name, localVarStatus.DEFINED))
  private def resolveLocal(expr: Expr, name: Token): Unit = // expr could be a variable or assignment
    boundary: // Scala 3 mechanism
      for i <- scopes.indices do
        if scopes(i).contains(name.lexeme) then
          interpreter.resolve(expr, i)
          scopes(i)(name.lexeme) = (name, localVarStatus.USED)
          break()
  private def resolveFunction(lambda: Lambda, fnType: FunctionType): Unit =
    val enclosingFunction: FunctionType = currentFunction
    currentFunction = fnType
    beginScope()
    lambda.parameters.foreach(p =>
      declare(p)
      define(p)
    )
    resolve(lambda.body)
    endScope()
    currentFunction = enclosingFunction

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
    if scopes.nonEmpty && scopes.top.get(expr.name.lexeme).contains(localVarStatus.DECLARED) then
      Lox.error(expr.name, "Can't read local variable in its own initializer.")
    resolveLocal(expr, expr.name)
  private def visitFunctionDeclStmt(stmt: FunctionDec): Unit =
    declare(stmt.name)
    define(stmt.name)
    resolveFunction(Lambda(stmt.name, stmt.params, stmt.body, "function"), FunctionType.FUNCTION)
  private def visitLambdaExpr(lambda: Lambda): Unit =
    resolveFunction(lambda, FunctionType.FUNCTION)

  private def visitClassStmt(stmt: Class): Unit =
    val enclosingClass: ClassType = currentClass
    currentClass = ClassType.CLASS
    declare(stmt.name)
    define(stmt.name)

    stmt.superclass match
      case Some(superclass) =>
        if stmt.name.lexeme == superclass.name.lexeme then
          Lox.error(superclass.name, "A class can't inherit from itself.")
      case None =>

    stmt.superclass match
      case Some(superclass) =>
        currentClass = ClassType.SUBCLASS
        resolve(superclass)
      case None =>

    stmt.superclass match
      case Some(superclass) =>
        beginScope()
        scopes.head("super") = (null, localVarStatus.USED)
      case None =>

    beginScope()
    scopes.head("this") = (null, localVarStatus.USED) // used by default
    for (method <- stmt.methods) do
      var declaration: FunctionType = FunctionType.METHOD
      if method.name.lexeme == "init" then
        declaration = FunctionType.INITIALIZER
      resolveFunction(method.initializer.get.asInstanceOf[Lambda], FunctionType.METHOD)

    endScope()
    if stmt.superclass.isDefined then endScope()
    currentClass = enclosingClass

  private def visitSuperExpr(expr: Super): Unit =
    if currentClass == ClassType.NONE then
      Lox.error(expr.keyword, "Can't use 'super' outside of a class.")
    else if currentClass != ClassType.SUBCLASS then
      Lox.error(expr.keyword, "Can't use 'super' in class with no subclass.")
    resolveLocal(expr, expr.keyword)

  // Uninteresting Visits
  private def visitExpressionStmt(stmt: Expression): Unit = resolve(stmt.expression)
  private def visitIfStmt(stmt: If): Unit =
    resolve(stmt.condition)
    resolve(stmt.thenBranch)
    if stmt.elseBranch != null then resolve(stmt.elseBranch)

  private def visitPrintStmt(stmt: Print): Unit = resolve(stmt.expression)
  private def visitReturnStmt(stmt: Return): Unit =
    if currentFunction == FunctionType.NONE then
      Lox.error(stmt.keyword, "Can't return from top-level code.")

    if stmt.value != null then
      if currentFunction == FunctionType.INITIALIZER then
        Lox.error(stmt.keyword, "Can't return a value from an initializer")
      resolve(stmt.value)

  private def visitWhileStmt(stmt: While): Unit =
    val previousLoop: LoopType = withinLoop
    withinLoop = LoopType.WHILE
    resolve(stmt.condition)
    resolve(stmt.body)
    withinLoop = previousLoop


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

  private def visitGetExpr(expr: Get): Unit = resolve(expr.loxObject)
  private def visitSetExpr(expr: Set): Unit =
    resolve(expr.value)
    resolve(expr.loxObject)

  private def visitThisExpr(expr: This): Unit =
    if currentClass == ClassType.NONE then
      Lox.error(expr.keyword, "Can't use 'this' outside of a class.")
    else
      resolveLocal(expr, expr.keyword)
}


object Resolver{
  enum localVarStatus:
    case DECLARED, DEFINED, USED
  enum FunctionType:
    case NONE, FUNCTION, METHOD, INITIALIZER
  enum ClassType:
    case NONE, CLASS, SUBCLASS
  enum LoopType:
    case NONE, WHILE
}