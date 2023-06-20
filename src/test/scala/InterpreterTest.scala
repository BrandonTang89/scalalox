import lox.TokenType.IDENTIFIER
import org.scalatest.funsuite.AnyFunSuite
import lox.{Interpreter, Parser, RuntimeError, Scanner, Token}


class InterpreterTest extends AnyFunSuite {

  test("Interpreter Test Expressions 1"){
    val text: String = "(1+2) * 3 / (4+5) + 9"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == 10.0)
  }

  test("Interpreter Test Expressions 2: Comma") {
    val text: String = "(1+2) * 3, (4+5) + 9"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == 18.0)
  }

  test("Interpreter Test Expressions 3: Ternary Operator") {
    val text: String = "2 < 3 ? 5+10*4 > 6 ? 7 : 8 : 9 < 10 ? 11 : 12"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == 7.0)
  }

  test("Interpreter Test Expressions 4: Ternary Operator") {
    val text: String = "2 < 3 ? \"Hello \" + \"World\" : 5"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == "Hello World")
  }

  test("Interpreter Test Expressions 5: String") {
    val text: String = "2 > 3 ? \"Hello \" + \"World\" : 5"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == 5.0)
  }

  test("Interpreter Test Expressions 6: Equality Comparison") {
    val text: String = "\"hello\" == 1.0"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == false)
  }

  test("Interpreter Test Expressions 7: Ordered Comparison") {
    val text: String = "\"hello\" <= \"hello world\""
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == true)
  }

  test("Interpreter Test Divide By 0"){
    val text: String = "1/0"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assertThrows[RuntimeError]{
      val obj: Any = Interpreter().evaluate(parser.expression())
    }
  }

  test("Interpreter Test Compare Mismatch") {
    val text: String = "1 < \"hello\""
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assertThrows[RuntimeError] {
      val obj: Any = Interpreter().evaluate(parser.expression())
    }
  }

  test("Interpreter Test 8: Ordered Comparison") {
    val text: String = "\"hello\" <= \"hello world\""
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.expression())
    assert(obj == true)
  }

  test("Interpreter Test 9: Variables") {
    val text: String = "var a = 10; \n var s = \"hello\"; \n var b = a + 2;\n var c = a + b;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)) == 12.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "c", null, 1)) == 22.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "s", null, 1)) == "hello")
  }





}