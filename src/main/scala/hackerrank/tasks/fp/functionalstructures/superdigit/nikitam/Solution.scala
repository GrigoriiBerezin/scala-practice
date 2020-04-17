package hackerrank.tasks.fp.functionalstructures.superdigit.nikitam

import scala.io.StdIn

object Solution {

  def main(args: Array[String]): Unit = {
    val rawInput = StdIn.readLine
    val Seq(n, k) = rawInput.split(" ").toSeq

    val initialNumber =
      n.view
        .map(_.toString)
        .map(_.toLong)
        .sum

    val possibleSuperDigit = initialNumber.sumAllDigits * k.toInt

    println(findSuperDigit(possibleSuperDigit))
  }


  @scala.annotation.tailrec
  def findSuperDigit(number: Long): Long = {
    if (number < 10) number
    else findSuperDigit(number.sumAllDigits)
  }

  private implicit class RichLong(l: Long) {
    def sumAllDigits: Long = {
      @scala.annotation.tailrec
      def go(currNumber: Long, acc: Long): Long =
        if (currNumber < 10) acc + currNumber
        else go(currNumber / 10, acc + currNumber % 10)

      go(l, 0)
    }
  }
}