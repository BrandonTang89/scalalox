import org.scalatest.funsuite.AnyFunSuite
import lox.{Scanner,Parser,Interpreter, RuntimeError}


class InterpreterTest extends AnyFunSuite {

  test("Interpreter Test Basic 1"){
    val text: String = "(1+2) * 3 / (4+5) + 9"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.parse())
    assert(obj == 10.0)
  }

  test("Interpreter Test Basic 2: Comma") {
    val text: String = "(1+2) * 3, (4+5) + 9"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.parse())
    assert(obj == 18.0)
  }

  test("Interpreter Test Basic 3: Ternary Operator") {
    val text: String = "2 < 3 ? 5+10*4 > 6 ? 7 : 8 : 9 < 10 ? 11 : 12"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.parse())
    assert(obj == 7.0)
  }

  test("Interpreter Test Basic 4: Ternary Operator") {
    val text: String = "2 < 3 ? \"Hello \" + \"World\" : 5"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.parse())
    assert(obj == "Hello World")
  }

  test("Interpreter Test Basic 5: String") {
    val text: String = "2 > 3 ? \"Hello \" + \"World\" : 5"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.parse())
    assert(obj == 5.0)
  }

  test("Interpreter Test Basic 6: Equality Comparison") {
    val text: String = "\"hello\" == 1.0"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.parse())
    assert(obj == false)
  }

  test("Interpreter Test Basic 7: Ordered Comparison") {
    val text: String = "\"hello\" <= \"hello world\""
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val obj: Any = Interpreter().evaluate(parser.parse())
    assert(obj == true)
  }

  test("Interpreter Test Divide By 0"){
    val text: String = "1/0"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assertThrows[RuntimeError]{
      val obj: Any = Interpreter().evaluate(parser.parse())
    }
  }

  test("Interpreter Test Compare Mismatch") {
    val text: String = "1 < \"hello\""
    val parser: Parser = Parser(Scanner(text).scanTokens())
    assertThrows[RuntimeError] {
      val obj: Any = Interpreter().evaluate(parser.parse())
    }
  }



}