package hackerrank.tasks.fp.functionalstructures.rpn.grisha

import scala.sys.process._

object ReversePolishNotation extends App {

  def exprToRPN(expr: String): List[String] = {
    def isDigit(term: String): Boolean =
      term matches "-*\\d+\\.*\\d*"

    def isLastOperation(term: String): Boolean =
      Seq("+", "-").contains(term)

    def isFirstOperation(term: String): Boolean =
      Seq("*", "/").contains(term)

    @scala.annotation.tailrec
    def getRPN(inputExpr: List[String],
               operationStack: List[String],
               stackRPN: List[String]): List[String] =
      operationStack match {
        case o :: os =>
          inputExpr match {
            case x :: xs if isDigit(x) =>
              getRPN(xs, operationStack, stackRPN :+ x)
            case x :: xs if isLastOperation(x) =>
              if (o != "(") getRPN(inputExpr, os, stackRPN :+ o) // 2
              else getRPN(xs, x :: operationStack, stackRPN) // 1
            case x :: xs if isFirstOperation(x) =>
              if (isDigit(o) || isLastOperation(o) || o == "(")
                getRPN(xs, x :: operationStack, stackRPN) // 1
              else getRPN(inputExpr, os, stackRPN :+ o) // 2
            case x :: xs if x == "(" =>
              getRPN(xs, x :: operationStack, stackRPN) // 1
            case x :: xs if x == ")" =>
              if (o != "(") getRPN(inputExpr, os, stackRPN :+ o) // 2
              else getRPN(xs, os, stackRPN) // 3
            case Nil =>
              if (o == "(") throw ExpressionError("Miss close bracket") // 5
              else getRPN(inputExpr, os, stackRPN :+ o) // 2
          }
        case Nil =>
          inputExpr match {
            case x :: xs if x == ")" =>
              throw ExpressionError("Wrong open bracket") // 5
            case x :: xs => getRPN(xs, List(x), stackRPN) // 1
            case Nil     => stackRPN // 4
          }
      }

    val terms = expr.split("\\s").toList
    getRPN(terms, Nil, Nil)
  }

  def calcRPN(stack: List[String]): Double =
    stack
      .foldLeft(List[Double]())(
        (acc, term) =>
          (acc, term) match {
            case (x :: y :: zs, "+") => (y + x) :: zs
            case (x :: y :: zs, "-") => (y - x) :: zs
            case (x :: y :: zs, "*") => (y * x) :: zs
            case (x :: y :: zs, "/") => (y / x) :: zs
            case (x :: y :: zs, "^") => Math.pow(y, x) :: zs // todo
            case (_, _)              => term.toDouble :: acc
        }
      )
      .head

  def calcExpression(expr: String): Double =
    calcRPN(exprToRPN(expr))

  def cheatCalc(expr: String, scale: Int = 1): Double =
    (s"echo scale=$scale;$expr" #| "bc").!!.trim.toDouble

  println(calcExpression("2 + 2"))
  println(calcExpression("25 + 4 * ( 3 - 1 ) / ( 2 * ( 5 + 2 ) )"))
}

case class ExpressionError(message: String) extends Error
