package lox
import lox.Parser.ParseError
import lox.TokenType.*

import collection.mutable.{ArrayBuffer, ArrayStack}
class Parser(val tokens: ArrayBuffer[Token], val parseExpressions: Boolean = false) {
  private var index: Int = 0
  /** Program Grammar
   * program → declaration* EOF; */
  def parse(): ArrayBuffer[Stmt] =
    val statements: ArrayBuffer[Stmt] = ArrayBuffer[Stmt]()
    while !isAtEnd do
      statements.addOne(declaration())
    statements

  private var current: Int = 0
  private def isAtEnd: Boolean = peek().tokenType == EOF
  private def advance(): Token = {
    if !isAtEnd then
      val c = tokens(current)
      current += 1
      c
    else previous() // return the last token before EOF
  }
  private def peek(): Token = {
    assert(current < tokens.length, "peek() out of range")
    tokens(current)
  }
  private def previous(): Token = {
    assert(current >= 1, "previous() out of range")
    tokens(current - 1)
  }
  private def check(tokenType: TokenType): Boolean = {
    if isAtEnd then false
    else peek().tokenType == tokenType
  }
  private def moveBack(): Token = {
    assert(current > 0)
    current -= 1
    tokens(current)
  }

  /** matches(ts) advances if the current tokenType is in ts */
  private def matches(types: TokenType*): Boolean = {
    var found = false
    for tokenType <- types do
      if !found && check(tokenType) then
        found = true
        advance()
    found
  }


  /** Declaration Grammar
   *  declaration → funDecl | varDecl | classDecl | statement */
  def declaration(): Stmt = {
    try
      if matches(VAR) then varDeclaration()
      else if matches(FUN) then function("function")
      else if matches(CLASS) then classDeclaration()
      else statement()
    catch
      case error: ParseError =>
        synchronize()
        null
  }

  /** Class Declaration
   *  classDecl → "class" IDENTIFIER "{" function* "}"; */
  private def classDeclaration(): Stmt = {
    val name: Token = consume(IDENTIFIER, "Expect class name.")
    consume(LEFT_BRACE, "Expect '{' before class body.")
    val methods: ArrayBuffer[Var] = ArrayBuffer[Var]()

    while !check(RIGHT_BRACE) && !isAtEnd do
      val fn = function("method").asInstanceOf[Var]
      methods.addOne(fn)

    consume(RIGHT_BRACE, "Expect '}' after class body.")
    Class(name, methods)
  }

  /** Fun Declaration
   *  funDecl → ("fun" function) | expressionStmt; // where the expressionStmt is one starting with "fun"
   *  namedFunction -> IDENTIFIER lambdaFunction;
   *  lambdaFunction ->  "(" parameters? ")" block;
   *  parameters → IDENTIFIER ( "," IDENTIFIER )*;
   */

  private def function(kind: String): Stmt = {
    var name: Token = null
    if check(IDENTIFIER) then
      name = consume(IDENTIFIER, "") // absorb the name
      val lambda: Lambda = lambdaFunction(kind)
      Var(name, Some(lambda))
    else if kind == "fun" then
      // Expression Statement
      moveBack() // Get back to fun, with the knowledge that we aren't at a declaration
      expressionStatement()
    else // kind == "method", we require a method identifier
      throw error(previous(), "Expected method identifier.")
  }


  /** Var Declaration Grammar
   *  varDecl → "var" IDENTIFIER ("=" expression)? : */

  private def varDeclaration(): Stmt = {
    val name: Token = consume(IDENTIFIER, "Expect variable name.")
    var initializer: Option[Expr] = None
    if matches(EQUAL) then
      initializer = Some(expression())

    consume(SEMICOLON, "Expect ';' after variable declaration")
    Var(name, initializer)
  }

  /** Statement Grammar
   *  statement → exprStmt | ifStmt | printStmt |
   *              forStmt | whileStmt | breakStatement | continueStatement |
   *              returnStatement | block;
   */
  private def statement(): Stmt = {
    if matches(PRINT) then printStatement()
    else if matches(IF) then ifStatement()
    else if matches(FOR) then forStatement()
    else if matches(WHILE) then whileStatement()
    else if matches(LEFT_BRACE) then Block(block())
    else if matches(BREAK) then breakStatement()
    else if matches(CONTINUE) then continueStatement()
    else if matches(RETURN) then returnStatement()
    else expressionStatement()
  }

  private def returnStatement(): Stmt = {
    val keyword: Token = previous()
    var value: Expr = null
    if !check(SEMICOLON) then value = expression()
    consume(SEMICOLON, "Expect ';' after return value.")
    Return(keyword, value)
  }


  /** Break and Continue
   *  breakStatement → "break" ";";
   *  continueStatement → "continue" ";";*/
  private def breakStatement(): Stmt = {
    val keyword: Token = previous()
    consume(SEMICOLON, "Expect ';' after 'break'.")
    Break(keyword)
  }

  private def continueStatement(): Stmt = {
    val keyword: Token = previous()
    consume(SEMICOLON, "Expect ';' after 'continue'.")
    Continue(keyword)
  }

  /** ForStmt Grammar
   * forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ; */
  private def forStatement(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    var initializer: Stmt = null
    if matches(SEMICOLON) then initializer = null
    else if matches(VAR) then initializer = varDeclaration()
    else initializer = expressionStatement()

    var condition: Expr = null
    if !check(SEMICOLON) then condition = expression()
    consume(SEMICOLON, "Expect ';' after loop condition.")

    var increment: Expr = null
    if !check(RIGHT_PAREN) then increment = expression()
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")

    var body: Stmt = statement()

    // Desugaring
    if increment != null then
      body = Block(ArrayBuffer(body, Expression(increment)))

    if condition == null then condition = Literal(true)
    body = While(condition, body)

    if initializer != null then
      body = Block(ArrayBuffer(initializer, body))

    body
  }

  /** whileStatement Grammar
   * whileStmt → "while" "(" expression ")" loopBodyStmt;
   */
  private def whileStatement(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition: Expr = expression()
    consume(RIGHT_PAREN, "Expect ')' after while condition.")
    val body: Stmt = statement()
    While(condition, body)
  }


  /** IfStmt Grammar
   * ifStmt → "if" "(" expression ")" statement ("else" statement)?;
   *
   * When we parse an else, we associate it with the lowest if*/
  private def ifStatement(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition: Expr = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")
    val thenBranch: Stmt = statement()
    var elseBranch: Stmt = null
    if matches(ELSE) then
      elseBranch = statement()
    If(condition, thenBranch, elseBranch)
  }

  /** Block Grammar
   *  block → "{" declaration* "}"; */
  private def block(): ArrayBuffer[Stmt] = { // assumes the '{' is already consumed
    val statements: ArrayBuffer[Stmt] = ArrayBuffer[Stmt]()
    while !check(RIGHT_BRACE) && !isAtEnd do
      statements.addOne(declaration())
    consume(RIGHT_BRACE, "Expected '}' after block.")
    statements
  }

  /** printStmt Grammar
   * exprStmt → "print" expression ";" */
  private def printStatement(): Stmt = {
    val value: Expr = expression()
    consume(SEMICOLON, "Expect ';' after print value.")
    Print(value)
  }

  /** exprStmt Grammar
   * exprStmt → expression ";" */
  private def expressionStatement(): Stmt = {
    val expr: Expr = expression()
    if check(SEMICOLON) || !parseExpressions then
      consume(SEMICOLON, "Expect ';' after expression.")
      Expression(expr)
    else Print(expr)
  }

  /** Expression Grammar
   *  expression → assignment (, expression)*;
   *
   *  The comma operator is left associative so
   *   a = 1, b = 2, c = 3 is ((a = 1, b = 2), c = 3) returning 3
   */
  def expression(): Expr = {
    var expr: Expr = assignment()
    while matches(COMMA) do
      val operator: Token = previous()
      val right: Expr = expression()
      expr = Binary(expr, operator, right)
    expr
  }

  /** Assignment Grammar
   *  assignment → (call ".")? IDENTIFIER "=" assignment | Ternary
   *
   *  Right associative
   *  Allows for multiple assignments: a = b = c = 10 is a = (b = (c = 10))
   */

  private def assignment(): Expr = {
    val expr: Expr = ternary()

    if matches(EQUAL) then
      val equals: Token = previous()
      val value: Expr = assignment()

      expr match
        case expr: Variable =>
          val name: Token = expr.name
          index += 1
          Assign(name, value, index-1)

        case get: Get => // assign to a field of a class
          Set(get.loxObject, get.name, value)

        case _ =>
          error(equals, "Invalid assignment target.")
          expr

    else expr
  }

  /** Ternary Grammar
   *  ternary → logic_or ("?" ternary ":" ternary))?;
   *
   * var x = 2 > 3 ? 5 < 6 ? 7 : 8 : 9 < 10 ? 11 : 12
   * is parsed as
   * var x = (2 > 3) ? (5 < 6 ? 7 : 8) : (9 < 10 ? 11 : 12)*/
  private def ternary(): Expr = {
    var expr: Expr = or()
    if matches(QUESTION_MARK) then
      val operator1: Token = previous()
      val middle: Expr = ternary()
      val operator2: Token = consume(COLON, "expected : after ?.")
      val right: Expr = ternary()
      expr = Ternary(expr, operator1, middle, operator2, right)
    expr
  }

  /** Logic-or Grammar
   *  logic_or → logic_and ( "or" logic_and )* ;
   */

  private def or(): Expr = {
    var expr: Expr = and()
    while matches(OR) do
      val operator: Token = previous()
      val right: Expr = and()
      expr = Logical(expr, operator, right)
    expr
  }

  /** Logic_and Grammar
   *  Logic_and → equality ( "and" equality )* ;
   */
  private def and(): Expr = {
    var expr: Expr = equality()
    while matches(AND) do
      val operator: Token = previous()
      val right: Expr = and()
      expr = Logical(expr, operator, right)
    expr
  }

  /** Equality Grammar
   *  equality → comparison ( ( "!=" | "==" ) comparison )* ; */
  private def equality(): Expr = {
    var expr: Expr = comparison()
    while matches(BANG_EQUAL, EQUAL_EQUAL) do // left associative i.e. a == b == c is ((a == b) == c)
      val operator: Token = previous()
      val right: Expr = comparison()
      expr = Binary(expr, operator, right)
    expr
  }

  /** Comparison Grammar
   *  comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ; */
  private def comparison(): Expr = {
    var expr: Expr = term()
    while matches(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) do
      val operator: Token = previous()
      val right: Expr = term()
      expr = Binary(expr, operator, right)
    expr
  }

  /** Term Grammar
   *  term → factor ( ( "-" | "+" ) factor )* ; */
  private def term(): Expr = {
    var expr: Expr = factor()
    while matches(MINUS, PLUS) do
      val operator: Token = previous()
      val right: Expr = factor()
      expr = Binary(expr, operator, right)
    expr
  }

  /** Factor Grammar
   *  factor → unary ( ( "/" | "*" ) unary )* ; */
  private def factor(): Expr = {
    var expr: Expr = unary()
    while matches(SLASH, STAR) do
      val operator: Token = previous()
      val right: Expr = unary()
      expr = Binary(expr, operator, right)
    expr
  }


  /** Unary Grammar
   * unary → ( "!" | "-" ) unary | call ; */
  private def unary(): Expr = {
    if matches(BANG, MINUS) then
      val operator: Token = previous()
      val right = unary()
      Unary(operator, right)
    else call()
  }
  /** Call Grammar
   *  call → primary ("(" arguments? ")" | "." IDENTIFIER )*;
   *  arguments -> assignment ("," assignment)*;
   */
  private def call(): Expr = {
    var expr: Expr = primary()
    while true do
      if matches(LEFT_PAREN) then
        expr = finishCall(expr)
      else if matches(DOT) then
        val name: Token = consume(IDENTIFIER, "Expect property name after '.' .")
        expr = Get(expr, name)
      else return expr
    null // unreached
  }
  private def finishCall(callee: Expr): Expr = {
    val arguments: ArrayBuffer[Expr] = ArrayBuffer[Expr]()
    if !check(RIGHT_PAREN) then
      while
        if arguments.size >= 255 then error(peek(), "Can't have more than 255 arguments.")
        // Report but don't throw error
        arguments.addOne(assignment()) // avoid allowing comma operator
        matches(COMMA)
      do()
    val paren: Token = consume(RIGHT_PAREN, "Expect ')' after arguments.")
    Call(callee, paren, arguments)
  }

  /** Primary Grammar
   * primary → NUMBER | STRING | "true" | "false" | "nil" |"(" expression ")" | IDENTIFIER | lambda; */
  private def primary(): Expr = {
    if matches(FALSE) then Literal(false)
    else if matches(TRUE) then Literal(true)
    else if matches(NIL) then Literal(null)
    else if matches(NUMBER, STRING) then Literal(previous().literal)
    else if matches(THIS) then
      index += 1
      This(previous(), index-1)
    else if matches(IDENTIFIER) then
      index += 1
      Variable(previous(), index-1)
    else if matches(LEFT_PAREN) then
      val expr: Expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)
    else if matches(FUN) then lambdaFunction("lambda function")

    else if matches(COMMA) then
      val operator: Token = previous()
      expression()
      throw error(operator, "comma operator unexpected.")
    else if matches(BANG_EQUAL, EQUAL_EQUAL) then
      val operator: Token = previous()
      comparison()
      throw error(operator, "equality operator unexpected.")
    else if matches(LESS,LESS_EQUAL,GREATER,GREATER_EQUAL) then
      val operator: Token = previous()
      term()
      throw error(operator, "comparison operator unexpected.")
    else if matches(PLUS,MINUS) then
      val operator: Token = previous()
      factor()
      throw error(operator, "plus minus operator unexpected.")
    else if matches(SLASH,STAR) then
      val operator: Token = previous()
      unary()
      throw error(operator, "star slash operator unexpected.")

    else throw error(peek(), "Expect expression.")
  }
  private def lambdaFunction(kind: String): Lambda = {
    val keyword: Token = previous() // either 'fun' (for anonymous functions) or function/method name
    //println("kind " + kind)
    // println("name " + keyword.lexeme)

    var fnType: String = kind
    val parameters: ArrayBuffer[Token] = ArrayBuffer[Token]()
    if kind != "method" || peek().tokenType == LEFT_PAREN then
      // println("normal Method")
      consume(LEFT_PAREN, "Expect '(' after " + kind + " name or declaration.")
      if !check(RIGHT_PAREN) then
        while
          if parameters.size >= 255 then
            error(peek(), "Can't have more than 255 parameters.")
          parameters.addOne(consume(IDENTIFIER, "Expect parameter name."))
          matches(COMMA)
        do ()
      consume(RIGHT_PAREN, "Expect ')' after parameters.")
    else
      fnType = "getter"
    consume(LEFT_BRACE, "Expect '{' before " + kind + " body.")
    val body: ArrayBuffer[Stmt] = block()
    Lambda(keyword, parameters, body, fnType)
  }

  // Parser Error Handling
  /** Expects a specific tokenType, consumes it */
  private def consume(tokenType: TokenType, message: String): Token = {
    if check(tokenType) then advance()
    else throw error(peek(), message)
  }
  /** error produces a ParseError while reporting to the user */
  private def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    ParseError()
  }
  private def synchronize(): Unit = {
    var done: Boolean = false
    while !isAtEnd && !done do
      if previous().tokenType == SEMICOLON then done = true
      peek().tokenType match
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => done = true
        case _ =>
      if !done then advance()
    // post: Either at end || peek() is a one of the key words || just past a ;
  }

}

object Parser{
  class ParseError() extends RuntimeException
}