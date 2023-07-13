package lox

import scala.collection.mutable.ArrayBuffer
class LoxFunction(val name: Token, val lambda: Lambda, val closure: Environment, val isInitializer: Boolean) extends LoxCallable {

  def bind(instance: LoxInstance): LoxFunction = // allows binding "this" to methods
    val environment: Environment = Environment(closure)
    environment.define("this", instance)
    LoxFunction(name, lambda, environment, isInitializer)

  def call(interpreter: Interpreter, arguments: ArrayBuffer[Any]): Any = {
    val environment: Environment = Environment(closure)
    for i <- lambda.parameters.indices do
      environment.define(lambda.parameters(i).lexeme, arguments(i))
    try
      interpreter.executeBlock(lambda.body, environment)

      if isInitializer then closure.getAt(0, Token(TokenType.THIS, "this", -1, -1))
      else null
    catch
      case ret: ReturnException =>
        if isInitializer then closure.getAt(0, Token(TokenType.THIS, "this", -1, -1))
        else ret.value
  }
  def arity: Int = lambda.parameters.size
  override def toString: String = "<fn > " + name.lexeme + ">"
}
