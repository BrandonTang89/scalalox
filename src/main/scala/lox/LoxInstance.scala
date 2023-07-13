package lox
import collection.mutable
class LoxInstance(private val klass: LoxClass) {
  override def toString: String = klass.name + " instance"
  private val fields: mutable.Map[String, Any] = mutable.Map[String, Any]()

  def get(name: Token, interpreter: Interpreter): Any =
    if fields.contains(name.lexeme) then fields(name.lexeme)
    // note that a field will shadow a method with the same name
    else
      val method: Option[LoxFunction] = klass.findMethod(name.lexeme)
      method match
        case Some(met) =>
          if met.lambda.fnType == "method" then
            met.bind(this)
          else met.bind(this).call(interpreter, mutable.ArrayBuffer[Any]())
        case None => throw RuntimeError(name, "Undefined property '" + name.lexeme + "'.")

  def set(name: Token, value: Any): Unit = fields(name.lexeme) = value
}
