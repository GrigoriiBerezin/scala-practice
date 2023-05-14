package hackerrank.tasks.fp.sortedarray

import scala.annotation.tailrec

object FindTwoElementsWithSum extends App {

  def calculate(array: Vector[Int], sum: Int): Option[(Int, Int)] = {
    @tailrec
    def helper(startIndex: Int, endIndex: Int): Option[(Int, Int)] =
      if (startIndex == endIndex) None
      else {
        val low = array(startIndex)
        val high = array(endIndex)
        val countSum = low + high
        if (countSum > sum) helper(startIndex, endIndex - 1)
        else if (countSum < sum) helper(startIndex + 1, endIndex)
        else Some((low, high))
      }

    if (array.isEmpty) None else helper(0, array.size - 1)
  }

  println(calculate(Examples.smallArray, 7))

}
