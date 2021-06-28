package hackerrank.tasks.fp.functionalstructures.superdigit.paveld

import scala.annotation.tailrec

object Solution {

  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    val str = readLine()
    val vals = str.split(" ")
    val digits: BigInt = BigInt(vals(0))
    val cont: Int = Integer.parseInt(vals(1))
    println(superDigit(digits, cont))
  }

  def superDigit(digits: BigInt, cont: Int): Int = {
    val accumulate = (acc: Int, char: Char) => acc + char.asDigit
    @tailrec
    def loop(digits: String): String = {
      if (digits.length == 1) digits
      else loop(digits.foldLeft(0)(accumulate).toString)
    }
    loop((digits * cont).toString).toInt
  }
}
