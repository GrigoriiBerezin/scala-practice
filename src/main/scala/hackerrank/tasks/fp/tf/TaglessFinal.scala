package hackerrank.tasks.fp.tf

object TaglessFinal extends App {
  object ExpressionProblem {
    sealed trait Expr
    case class B(b: Boolean) extends Expr
    case class Or(left: Expr, right: Expr) extends Expr
    case class And(left: Expr, right: Expr) extends Expr
    case class Not(expr: Expr) extends Expr

    val aGiantBoolean = Or(And(B(true), B(false)), B(false))

    def eval(expr: Expr): Boolean = expr match {
      case And(left, right) => eval(left) && eval(right)
      case B(b) => b
      case Not(expr) => !eval(expr)
      case Or(left, right) => eval(left) || eval(right)
    }
  }
}
