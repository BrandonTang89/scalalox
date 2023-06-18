import lox.{Token, TokenType, Binary, Expr, Literal, Unary, Grouping}
import org.scalatest.funsuite.AnyFunSuite

class ExprTest extends AnyFunSuite {
  test("AST Printing Test 1") {
    val expression: Expr = Binary(
      Unary(
        Token(TokenType.MINUS, "-", null, 1),
        Literal(123)
      ),
      Token(TokenType.STAR, "*", null, 1),
      Grouping(Literal(45.67))
    )
    assert(expression.toString() == "(* (- 123) (group 45.67))")
  }
}
