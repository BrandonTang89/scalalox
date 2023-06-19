package lox
import lox.Parser.ParseError
import lox.TokenType.*

import collection.mutable.ArrayBuffer
class Parser(val tokens: ArrayBuffer[Token]) {
  def parse(): Expr =
    try expression()
    catch
      case e: ParseError => null

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

  /** Expression Grammar
   *  expression → ternary (, expression)*;
   *
   *  The comma operator is left associative so
   *   a = 1, b = 2, c = 3 is ((a = 1, b = 2), c = 3) returning 3
   */
  private def expression(): Expr = {
    var expr: Expr = ternary()
    while matches(COMMA) do
      val operator: Token = previous()
      val right: Expr = expression()
      expr = Binary(expr, operator, right)
    expr
  }

  /** Ternary Grammar
   *  ternary → equality ? ternary : ternary;
   *
   * var x = 2 > 3 ? 5 < 6 ? 7 : 8 : 9 < 10 ? 11 : 12
   * is parsed as
   * var x = (2 > 3) ? (5 < 6 ? 7 : 8) : (9 < 10 ? 11 : 12)*/
  private def ternary(): Expr = {
    var expr = equality()
    if matches(QUESTION_MARK) then
      val operator1: Token = previous()
      val middle: Expr = ternary()
      val operator2: Token = consume(COLON, "expected : after ?.")
      val right: Expr = ternary()
      expr = Ternary(expr, operator1, middle, operator2, right)
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
   * primary → NUMBER | STRING | "true" | "false" | "nil" |"(" expression ")" ; */
  private def primary(): Expr = {
    if matches(FALSE) then Literal(false)
    else if matches(TRUE) then Literal(true)
    else if matches(NIL) then Literal(null)
    else if matches(NUMBER, STRING) then Literal(previous().literal)
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
  private class ParseError() extends RuntimeException
}