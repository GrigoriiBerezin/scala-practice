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
    def getRPN(leftTerms: List[String],
               operations: List[String],
               result: List[String]): List[String] =
      operations match {
        case o :: os =>
          leftTerms match {
            case x :: xs if isDigit(x) =>
              getRPN(xs, operations, result :+ x)
            case x :: xs if isLastOperation(x) =>
              if (o != "(") getRPN(leftTerms, os, result :+ o) // 2
              else getRPN(xs, x :: operations, result) // 1
            case x :: xs if isFirstOperation(x) =>
              if (isDigit(o) || isLastOperation(o) || o == "(")
                getRPN(xs, x :: operations, result) // 1
              else getRPN(leftTerms, os, result :+ o) // 2
            case x :: xs if x == "(" =>
              getRPN(xs, x :: operations, result) // 1
            case x :: xs if x == ")" =>
              if (o != "(") getRPN(leftTerms, os, result :+ o) // 2
              else getRPN(xs, os, result) // 3
            case Nil =>
              if (o == "(") throw ExpressionError("Miss close bracket") // 5
              else getRPN(leftTerms, os, result :+ o) // 2
          }
        case Nil =>
          leftTerms match {
            case x :: _ if x == ")" =>
              throw ExpressionError("Wrong open bracket") // 5
            case x :: xs => getRPN(xs, List(x), result) // 1
            case Nil     => result // 4
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
            case (x1 :: x2 :: xs, "+") => (x2 + x1) :: xs
            case (x1 :: x2 :: xs, "-") => (x2 - x1) :: xs
            case (x1 :: x2 :: xs, "*") => (x2 * x1) :: xs
            case (x1 :: x2 :: xs, "/") => (x2 / x1) :: xs
            case (x1 :: x2 :: xs, "^") => Math.pow(x2, x1) :: xs // todo
            case (_, _)                => term.toDouble :: acc
        }
      )
      .head

  def calcExpression(expr: String): Double =
    calcRPN(exprToRPN(expr))

  def cheatCalc(expr: String, scale: Int = 1): Double =
    (s"echo scale=$scale;$expr" #| "bc").!!.trim.toDouble

  println(calcRPN(List("1", "2", "3", "+", "*")))
  println(calcExpression("3 + -5"))
}

case class ExpressionError(message: String) extends Error
