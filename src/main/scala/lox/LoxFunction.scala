package lox

import scala.collection.mutable.ArrayBuffer
class LoxFunction(val name: Token, val lambda: Lambda, val closure: Environment) extends LoxCallable {
  def call(interpreter: Interpreter, arguments: ArrayBuffer[Any]): Any = {
    val environment: Environment = Environment(closure)
    for i <- lambda.parameters.indices do
      environment.define(lambda.parameters(i).lexeme, arguments(i))
    try
      interpreter.executeBlock(lambda.body, environment)
      null
    catch
      case ret: ReturnException => ret.value
  }
  def arity: Int = lambda.parameters.size
  override def toString: String = "<fn > " + name.lexeme + ">"
}
