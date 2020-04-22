package hackerrank.tasks.fp.functionalstructures.superdigit.grisha

object Solution {
  def super_digit(n: String, k: Int): Long = {
    def reducer(digit: String) =
      digit.split("").map(_.toLong).sum

    @scala.annotation.tailrec
    def simplify(digit: Long): Long =
      if (digit < 10) digit
      else simplify(reducer(digit.toString))

    simplify(k * reducer(n))
  }

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
     */
    val array = scala.io.StdIn.readLine().split(" ")
    println(super_digit(array(0), array(1).toInt))
  }
}
