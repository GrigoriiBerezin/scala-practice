package assessments.youtube

import scala.annotation.tailrec

object FindTwoElemWithGivenSum extends App {
  def calculate(array: Vector[Int], sum: Int): Option[(Int, Int)] = {
    @tailrec
    def inner(left: Int, right: Int): Option[(Int, Int)] = {
      if (right <= left) None
      else {
        val actualSum = array(left) + array(right)
        if (actualSum < sum) inner(left + 1, right)
        else if (actualSum > sum) inner(left, right - 1)
        else Some(array(left), array(right))
      }
    }

    inner(0, array.size - 1)
  }

  val example = Vector(-10, -5, -4, 0, 1, 5, 7, 10)

  println(calculate(example, 3))
}
