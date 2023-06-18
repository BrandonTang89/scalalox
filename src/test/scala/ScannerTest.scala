import lox.{Scanner, Token}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer
import lox.TokenType.*
class ScannerTest extends AnyFunSuite {

  private def isSameArrayBuffer[T](xs: ArrayBuffer[T], ys: ArrayBuffer[T]): Boolean = {
    if xs.length != ys.length then false
    else xs.zip(ys).forall((x, y) => x == y)
  }

  test("Test Scanner 'var x = 10'"){
    assert(isSameArrayBuffer(
      Scanner("var x = 10").scanTokens(),
      ArrayBuffer[Token](
        Token(VAR, "var", null, 1),
        Token(IDENTIFIER, "x", null, 1),
        Token(EQUAL, "=", null, 1),
        Token(NUMBER, "10", 10.toDouble, 1),
        Token(EOF, "", null, 1)
      )
    ))
  }
  test("Test Scanner 'var a = 1; while (a < 10) { print a; a = a + 1;}'"){
    assert(isSameArrayBuffer(
      Scanner("var a = 1;\nwhile (a < 10) {\n  print a;\n  a = a + 1;\n}").scanTokens(),
      ArrayBuffer[Token](
        Token(VAR, "var", null, 1),
        Token(IDENTIFIER, "a", null, 1),
        Token(EQUAL, "=", null, 1),
        Token(NUMBER, "1", 1.toDouble, 1),
        Token(SEMICOLON,";", null, 1),
        Token(WHILE, "while", null, 2),
        Token(LEFT_PAREN, "(", null, 2),
        Token(IDENTIFIER, "a", null, 2),
        Token(LESS, "<", null, 2),
        Token(NUMBER, "10", 10.toDouble, 2),
        Token(RIGHT_PAREN,")", null, 2),
        Token(LEFT_BRACE, "{", null, 2),
        Token(PRINT, "print", null, 3),
        Token(IDENTIFIER, "a", null, 3),
        Token(SEMICOLON,";", null, 3),
        Token(IDENTIFIER, "a", null, 4),
        Token(EQUAL, "=", null, 4),
        Token(IDENTIFIER, "a", null, 4),
        Token(PLUS, "+", null, 4),
        Token(NUMBER, "1", 1.toDouble, 4),
        Token(SEMICOLON,";", null, 4),
        Token(RIGHT_BRACE,"}", null, 5),
        Token(EOF, "", null, 5)
      )
    ))
  }
}
