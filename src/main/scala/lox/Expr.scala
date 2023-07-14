package lox
import collection.mutable.ArrayBuffer

trait Expr{

  // Pretty Printing for Debugging
  override def toString: String = {
    this match
      case Assign(n, v, i) => parenthesize("assignment " + n.lexeme, v)
      case Binary(l, o, r) => parenthesize(o.lexeme, l, r)
      case Grouping(e) => parenthesize("group", e)
      case Literal(v) => if v == null then "nil" else v.toString
      case Logical(l, o, r) => parenthesize(o.lexeme, l, r)
      case Ternary(l, o1, m, o2, r) => parenthesize(o1.lexeme + o2.lexeme, l, m, r)
      case Unary(o, r) => parenthesize(o.lexeme, r)
      case Variable(n, i) => n.lexeme
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
case class Assign(name: Token, value: Expr, index: Int) extends Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Ternary(left: Expr, operator1: Token, middle: Expr, operator2: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Logical(left: Expr, operator: Token, right: Expr) extends Expr
case class Literal(value: Any) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class Call(calle: Expr, paren: Token, arguments: ArrayBuffer[Expr]) extends Expr
case class Lambda(keyword: Token, parameters: ArrayBuffer[Token], body: ArrayBuffer[Stmt], fnType: String) extends Expr
case class Get(loxObject: Expr, name: Token) extends Expr
class LookUpable(val index: Int) extends Expr
case class Variable(name: Token, i: Int) extends LookUpable(i) // index for resolution
case class Set(loxObject: Expr, name:Token, value: Expr) extends Expr
case class Super(keyword: Token, method: Token, index: Int) extends Expr
case class This(keyword: Token, i: Int) extends LookUpable(i) // index for resolution
