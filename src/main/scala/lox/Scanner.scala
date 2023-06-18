package lox
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.HashMap
import TokenType.*
import jdk.nashorn.internal.ir.annotations.Immutable
class Scanner(val source: String) {
  val tokens: ArrayBuffer[Token] = ArrayBuffer[Token]()

  def scanTokens(): ArrayBuffer[Token] = {
    while !isAtEnd do
      start = current
      scanToken()
    tokens.addOne(Token(EOF, "", null, line))
    tokens
  }

  private def scanToken(): Unit = {
    val c: Char = advance()
    c match
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => addToken(if curMatch('=') then BANG_EQUAL else BANG)
      case '=' => addToken(if curMatch('=') then EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if curMatch('=') then LESS_EQUAL else LESS)
      case '>' => addToken(if curMatch('=') then GREATER_EQUAL else GREATER)

      case '/' =>
        if curMatch('/') then
            while peek() != '\n' && !isAtEnd do advance()
        else addToken(SLASH)

      case ' ' | '\r' | '\t' => // ignore whitespace
      case '\n' => line += 1

      // Literals
      case '"' => string()
      case c if isDigit(c) => number()
      case c if isAlpha(c) => identifier()

      // Error
      case _ => Lox.error(line, "Unexpected Character.")
  }

  // Methods for Manipulating the current position
  private var start: Int = 0
  private var current: Int = 0
  private var line: Int = 1
  private def isAtEnd: Boolean = current >= source.length
  private def advance(): Char = {
    assert(current <= source.length)
    val c = source(current)
    current += 1
    c
  }
  private def peek(): Char = {
    if isAtEnd then '\u0000'
    else source(current)
  }
  private def peekNext(): Char = {
    if current + 1 < source.length then source(current + 1)
    else '\u0000'
  }
  private def curMatch(expected: Char) = {
    if !isAtEnd && source(current) == expected then
      current += 1
      true
    else false
  }

  // Generic Methods
  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
  private def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
  private def isAlphaNumeric(c: Char): Boolean = isDigit(c) || isAlpha(c)

  // Methods for Strings, Numbers, Identifiers / Literals
  private def string(): Unit = {
    while !isAtEnd && peek() != '"' do
      if peek() == '\n' then line += 1
      advance()

    if isAtEnd then Lox.error(line, "Unterminated String.")
    else
        advance() // the closing "
        val value = source.substring(start+1, current-1)
        addToken(STRING, value)
  }
  private def number(): Unit = {
    while isDigit(peek()) do advance()
    if peek() == '.' && isDigit(peekNext()) then
      advance()
      while isDigit(peek()) do advance()
    addToken(NUMBER, source.substring(start, current).toDouble)
  }
  private def identifier(): Unit = {
    while isAlphaNumeric(peek()) do advance()
    val text: String = source.substring(start, current)
    val tokenType: Option[TokenType] = Scanner.keywords.get(text)
    tokenType match
      case None => addToken(IDENTIFIER)
      case Some(keyword) => addToken(keyword)
  }

  // Methods to add Tokens to List
  private def addToken(tokenType: TokenType): Unit = addToken(tokenType, null)
  private def addToken(tokenType: TokenType, literal: Any): Unit = {
    val text: String = source.substring(start, current)
    tokens.addOne(Token(tokenType, text, literal, line))
  }
}

object Scanner{
  private val keywords: HashMap[String, TokenType] = HashMap(
    ("and", AND),
    ("class", CLASS),
    ("else", ELSE),
    ("false", FALSE),
    ("for", FOR),
    ("fun", FUN),
    ("if", IF),
    ("nil", NIL),
    ("or", OR),
    ("print", PRINT),
    ("return", RETURN),
    ("super", SUPER),
    ("this", THIS),
    ("true", TRUE),
    ("var", VAR),
    ("while", WHILE),
  )
}
