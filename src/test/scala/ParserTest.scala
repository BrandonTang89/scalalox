import lox.TokenType.{EOF, LEFT_PAREN, MINUS, NUMBER, RIGHT_PAREN, STAR}
import lox.{Parser, RuntimeError, Scanner, Token, Lox}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class ParserTest extends AnyFunSuite {
  test("Parser Test Basic 1") {
    val tokens = ArrayBuffer[Token](
      Token(MINUS, "-", null, 1),
      Token(NUMBER, "123", 123, 1),
      Token(STAR, "*", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(NUMBER, "45.67", 45.67, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(EOF, "", null, 1)
    )
    assert(Parser(tokens).expression().toString() == "(* (- 123) (group 45.67))")
  }

  test("Parser Test Basic Fail 1") {
    val tokens = ArrayBuffer[Token](
      Token(MINUS, "-", null, 1),
      Token(NUMBER, "123", 123, 1),
      Token(STAR, "*", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(NUMBER, "45.67", 45.67, 1),
      //Token(RIGHT_PAREN, ")", null, 1),
      Token(EOF, "", null, 1)
    )
    assertThrows[lox.Parser.ParseError] {
      Parser(tokens).expression()
    }
  }

  test("Parser Test Basic Fail 2") {
    val tokens = ArrayBuffer[Token](
      //Token(MINUS, "-", null, 1),
      //Token(NUMBER, "123", 123, 1),
      Token(STAR, "*", null, 1),
      Token(LEFT_PAREN, "(", null, 1),
      Token(NUMBER, "45.67", 45.67, 1),
      Token(RIGHT_PAREN, ")", null, 1),
      Token(EOF, "", null, 1)
    )
    assertThrows[lox.Parser.ParseError]{
      Parser(tokens).expression()
    }
  }

  test("Scan-Parse Integration Test 1"){
    val text: String = "\"hello world\" + \"is cool\""
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assert(parser.expression().toString == "(+ hello world is cool)")
  }

  test("Scan-Parse Integration Test 2") {
    val text: String = "(1+2) * 3 / (4+5) + 9"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assert(parser.expression().toString == "(+ (/ (* (group (+ 1.0 2.0)) 3.0) (group (+ 4.0 5.0))) 9.0)")
  }

  test("Scan-Parse Integration Test 3") {
    val text: String = "(1+2) * 3, (4+5) + 9"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assert(parser.expression().toString == "(, (* (group (+ 1.0 2.0)) 3.0) (+ (group (+ 4.0 5.0)) 9.0))")
  }

  test("Scan-Parser Integration Test 4: Ternary Operator"){
    val text: String = "2 > 3 ? 5+10*4 < 6 ? 7 : 8 : 9 < 10 ? 11 : 12"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assert(parser.expression().toString == "(?: (> 2.0 3.0) (?: (< (+ 5.0 (* 10.0 4.0)) 6.0) 7.0 8.0) (?: (< 9.0 10.0) 11.0 12.0))")
  }

  test("Scan-Parser Integration Test 5: Ternary Operator") {
    val text: String = "(2 < 3 ? true : false) ? 5 < 6 ? 7 : 8 : 9 < 10 ? 11 : 12"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assert(parser.expression().toString == "(?: (group (?: (< 2.0 3.0) true false)) (?: (< 5.0 6.0) 7.0 8.0) (?: (< 9.0 10.0) 11.0 12.0))")
  }

  test("Scan-Parser Integration Test 6: Variable") {
    val text: String = "x + y + 3.0"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assert(parser.expression().toString == "(+ (+ x y) 3.0)")
  }

}
