import lox.Lox.interpreter
import lox.TokenType.IDENTIFIER
import org.scalatest.funsuite.AnyFunSuite
import lox.{Interpreter, Parser, Resolver, RuntimeError, Scanner, Stmt, Token}

import scala.collection.mutable.ArrayBuffer


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

  test("Interpreter Test 9: Variable Declaration") {
    val text: String = "var a = 10; \n var s = \"hello\"; \n var b = a + 2;\n var c = a + b;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)) == 12.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "c", null, 1)) == 22.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "s", null, 1)) == "hello")
  }

  test("Interpreter Test 10: Variable Assignment") {
    val text: String = "var a = 10; \n var b = 10;\n a = a + b;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 20.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)) == 10.0)
  }

  test("Interpreter Test 11: Variable Use Before Definition") {
    val text: String = "var a; var b = c + 10;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()

    assertThrows[RuntimeError] {
      interpreter.execute(parser.declaration())
      interpreter.execute(parser.declaration())
    }
  }

  test("Interpreter Test 12: Variable Use Before Initialization") {
    val text: String = "var a; var b = a + 10;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()

    assertThrows[RuntimeError] {
      interpreter.execute(parser.declaration())
      interpreter.execute(parser.declaration())
    }
  }

  test("Interpreter Test 13: Variable Assignment + Ternary Operator") {
    val text: String = "var a = 10; var b = a > 9 ? a + a : 0;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10.0)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)) == 20.0)
  }

  test("Interpreter Test 14: If Conditional") {
    val text: String = "var a; if (true) a = 10; else a = 20;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10.0)
  }

  test("Interpreter Test 14.1: If Conditional") {
    val text: String = "var a; if (false) a = 10; else a = 20;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 20.0)
  }

  test("Interpreter Test 15: Logical Or") {
    val text: String = "var a=0; if ((a = 10) or (a = 20)) 1;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10.0)
  }

  test("Interpreter Test 15.1: Logical Or") {
    val text: String = "var a=0; if ((a = false) or (a = 20)) 1;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 20.0)
  }

  test("Interpreter Test 16: Logical And") {
    val text: String = "var a=0; if ((a = 10) and (a = 20)) 1;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val interpreter: Interpreter = Interpreter()
    interpreter.interpret(parser.parse())
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 20.0)
  }

  test("Interpreter Test 17: For Loop") {
    val text: String = "var a = 1; for (var i = 0; i < 10; i = i + 1) a = a * 2;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 1024)
  }

  test("Interpreter Test 18: While Loop") {
    val text: String = "var a = 1; while (a < 10) a = a + 1;"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10)
  }

  test("Interpreter Test 19: While Loop with Break") {
    val text: String = "var a = 1; while (a < 10) {a = a + 1; if (a == 5) break;}"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 5)
  }

  test("Interpreter Test 19.1: While Loop with Break") {
    val text: String = "var a = 1; var b = 1; while (a < 10) {while (b < 10) {if (b == 5) break; else b = b + 1;} a = a + 1; }"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)) == 5)
  }

  test("Interpreter Test 20: While Loop with Continue") {
    val text: String = "var a = 0; var b = 0; while (a < 10) {a = a + 1; if (a <= 5) continue; b = b + 1;  }"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)) == 10)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)) == 5)
  }

  test("Interpreter Test 21: Calling Native Functions") {
    val text: String = "var a = clock() - clock();"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] < 1)
  }

  test("Interpreter Test 22: Declaring and Using Functions") {
    val text: String = "fun f(a){return a+2;} var a = f(1);"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] == 3)
  }

  test("Interpreter Test 23: Recursive Functions") {
    val text: String = "fun f(n){if (n <= 1) return n; return f(n-2) + f(n-1);} var a = f(5);"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] == 5)
  }

  test("Interpreter Test 24: Closures") {
    val text: String = "fun makeCounter() {var i = 0;fun count(){i = i + 1;return i;}return count;}" +
                       "var counter = makeCounter();var a = counter();var b =counter();"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] == 1)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)).asInstanceOf[Double] == 2)
  }

  test("Interpreter Test 25: Lambda Functions") {
    val text: String = "var a = fun (x) {return x*2;}(2);"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] == 4)
  }

   test ("Interpreter Test 25.1: Lambda Functions") {
    val text: String = "var a = fun (x) {return x*2;}; var b = a(10);"
     val parser: Parser = Parser(Scanner(text).scanTokens())
     val statements = parser.parse()
     val interpreter: Interpreter = Interpreter()
     val resolver: Resolver = Resolver(interpreter)
     resolver.resolve(statements)
     interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)).asInstanceOf[Double] == 20)
  }

  test("Interpreter Test 25.2: Lambda Functions") {
    val text: String = "fun thrice(fn) {var x = 1; for (var i = 1; i <= 3; i = i + 1) {x = fn(x);} return x;}" +
                       "var a = thrice(fun (a) {return a*2;});"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] == 8)
  }

  test("Interpreter Test 25.3: Lambda Functions") {
    val text: String = "var a = (1 < 2 ? fun (a){return a*2;} : fun(a){return a/2;})(1);"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] == 2)
  }

  test("Interpreter Test 25.4: Lambda Functions") {
    val text: String = "var f = fun(a){if (a<=1) return a; return f(a-1) + f(a-2);}; var a = f(5);"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "a", null, 1)).asInstanceOf[Double] == 5)
  }

  test("Interpreter Test 26: Properly Closed Functions") {
    val text: String = "var a = \"global\"; var b; var c;{fun retA() {return a;}b = retA();var a = \"block\";c = retA();}"
    val parser: Parser = Parser(Scanner(text).scanTokens())
    val statements = parser.parse()
    val interpreter: Interpreter = Interpreter()
    val resolver: Resolver = Resolver(interpreter)
    resolver.resolve(statements)
    interpreter.interpret(statements)
    assert(interpreter.environment.get(Token(IDENTIFIER, "b", null, 1)) == "global")
    assert(interpreter.environment.get(Token(IDENTIFIER, "c", null, 1)) == "global")

  }

}