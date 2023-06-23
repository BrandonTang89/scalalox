package lox
import lox.Parser.ParseError
import lox.TokenType.*

import collection.mutable.ArrayBuffer
class Parser(val tokens: ArrayBuffer[Token], val parseExpressions: Boolean = false) {

  private var withinLoop: Int = 0
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
   *  declaration → varDecl | statement */
  def declaration(): Stmt = {
    try
      if matches(VAR) then varDeclaration()
      else statement()
    catch
      case error: ParseError =>
        synchronize()
        null
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
   *  statement → exprStmt | ifStmt | printStmt | forStmt | whileStmt | breakStatement | continueStatement | block;
   *  break and continue statements can only be written for statements within a loop body
   *  (checked with a global variable).
   */
  private def statement(): Stmt = {
    if matches(PRINT) then printStatement()
    else if matches(IF) then ifStatement()
    else if matches(FOR) then forStatement()
    else if matches(WHILE) then whileStatement()
    else if matches(LEFT_BRACE) then Block(block())
    else if matches(BREAK) then breakStatement()
    else if matches(CONTINUE) then continueStatement()
    else expressionStatement()
  }


  /** Break and Continue
   *  breakStatement → "break" ";";
   *  continueStatement → "continue" ";";*/
  private def breakStatement(): Stmt = {
    if withinLoop > 0 then
      consume(SEMICOLON, "Expect ';' after 'break'.")
      Break()
    else
      error(previous(), "Unexpected 'break' outside a loop.")
      null
  }

  private def continueStatement(): Stmt = {
    if withinLoop > 0 then
      consume(SEMICOLON, "Expect ';' after 'continue'.")
      Continue()
    else
      error(previous(), "Unexpected 'continue' outside a loop.")
      null
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

    withinLoop += 1
    var body: Stmt = statement()
    withinLoop -= 1

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
    withinLoop += 1
    val body: Stmt = statement()
    withinLoop -= 1
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
  private def block(): ArrayBuffer[Stmt] = {
    val statements: ArrayBuffer[Stmt] = ArrayBuffer[Stmt]()
    while !check(RIGHT_BRACE) && !isAtEnd do
      statements.addOne(declaration())
    consume(RIGHT_BRACE, "Expected ')' after block.")
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
   *  assignment → IDENTIFIER "=" assignment | Ternary
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
          Assign(name, value)
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
   * unary → ( "!" | "-" ) unary | primary ; */
  private def unary(): Expr = {
    if matches(BANG, MINUS) then
      val operator: Token = previous()
      val right = unary()
      Unary(operator, right)
    else primary()
  }

  /** Primary Grammar
   * primary → NUMBER | STRING | "true" | "false" | "nil" |"(" expression ")" | IDENTIFIER; */
  private def primary(): Expr = {
    if matches(FALSE) then Literal(false)
    else if matches(TRUE) then Literal(true)
    else if matches(NIL) then Literal(null)
    else if matches(NUMBER, STRING) then Literal(previous().literal)
    else if matches(IDENTIFIER) then Variable(previous())
    else if matches(LEFT_PAREN) then
      val expr: Expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Grouping(expr)

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