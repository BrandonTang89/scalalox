package lox

trait Expr{

  // Pretty Printing for Debugging
  override def toString: String = {
    this match
      case Binary(l, o, r) => parenthesize(o.lexeme, l, r)
      case Grouping(e) => parenthesize("group", e)
      case Literal(v) => if v == null then "nil" else v.toString
      case Unary(o, r) => parenthesize(o.lexeme, r)
      case Ternary(l, o1, m, o2, r) => parenthesize(o1.lexeme + o2.lexeme, l, m, r)
  }
  private def parenthesize(name: String, expressions: Expr*): String = {
    val sb: StringBuilder = StringBuilder("(").append(name)
    for expr <- expressions do
      sb.append(' ')
      sb.append(expr.toString)
    sb.append(')')
    sb.toString()
  }

}

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Ternary(left: Expr, operator1: Token, middle: Expr, operator2: Token, right: Expr) extends Expr

case class Grouping(expression: Expr) extends Expr

case class Literal(value: Any) extends Expr

case class Unary(operator: Token, right: Expr) extends Expr
