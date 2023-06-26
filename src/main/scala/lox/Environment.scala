package lox
import scala.collection.mutable
class Environment(var enclosing: Environment = null) {
  private val values: mutable.HashMap[String, Option[Any]] = mutable.HashMap[String, Option[Any]]()

  def define(name: String, value: Any): Unit = values(name) = Some(value)
  def define(name: String): Unit = values(name) = None
  def getAt(distance: Int, name: Token): Any =
    ancestor(distance).values.get(name.lexeme) match
      case None => assert(false, "Cannot find variable at this distance")
      case Some(None) => throw RuntimeError(name, "Uninitialized variable '" + name + "'.")
      case Some(Some(v)) => v
  private def ancestor(distance: Int): Environment =
    var environment: Environment = this
    for i <- 0 until distance do
      environment = environment.enclosing
    environment
  def get(name: Token): Any =
    if values.contains(name.lexeme) then
      val v = values(name.lexeme)
      v match
        case Some(value) => value
        case None => throw RuntimeError(name, "Uninitialized variable '" + name.lexeme + "'.")
    else if enclosing != null then enclosing.get(name)
    else throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")

  def assignAt(distance: Int, name: Token, value: Any): Unit =
    ancestor(distance).values.put(name.lexeme, Some(value))

  def assign(name: Token, value: Any): Unit =
    if values.contains(name.lexeme) then values(name.lexeme) = Some(value)
    else if enclosing != null then enclosing.assign(name, value)
    else throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")

}
