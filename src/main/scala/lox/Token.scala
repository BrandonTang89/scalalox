package lox

case class Token(tokenType: TokenType,  lexeme: String, literal: Any, line: Int) {
  override def toString: String = {
    tokenType.toString + " " + lexeme + " " + literal
  }
}
