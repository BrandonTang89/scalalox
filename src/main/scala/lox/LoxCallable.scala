package lox

import scala.collection.mutable.ArrayBuffer
trait LoxCallable {
  def call(interpreter: Interpreter, arguments: ArrayBuffer[Any]): Any
  def arity: Int
}
