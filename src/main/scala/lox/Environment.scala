package lox
import scala.collection.mutable
class Environment {
  private val values: mutable.HashMap[String, Any] = mutable.HashMap[String, Any]()
  def define(name: String, value: Any): Unit = values(name) = value
  def get(name: Token): Any =
    val v = values.getOrElse(name.lexeme, null)
    v match
      case null => throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
      case _ => v
}
