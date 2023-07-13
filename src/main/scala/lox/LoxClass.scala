package lox
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class LoxClass(val name: String, val methods: mutable.Map[String, LoxFunction]) extends LoxCallable {


  override def toString: String = name

  override def arity: Int =
    val initializer: Option[LoxFunction] = findMethod("init")
    initializer match
      case Some(init) => init.arity
      case None => 0

  override def call(interpreter: Interpreter, arguments: ArrayBuffer[Any]): Any =
    val instance: LoxInstance = LoxInstance(this)

    val initializer: Option[LoxFunction] = findMethod("init")
    initializer match
      case Some(init) => init.bind(instance).call(interpreter, arguments)
      case None =>

    instance
  def findMethod(name: String): Option[LoxFunction] = methods.get(name)
}
