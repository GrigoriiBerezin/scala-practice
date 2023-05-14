package assessments.youtube

import scala.annotation.tailrec

object FindNextHottestDay extends App {
  val example = Vector(11, 9, 12, 15, 8, 6, 5, 16)

  def calculate(temperatures: Seq[Int]): Seq[Int] = {
    @tailrec
    def helper(indexValueSource: (Int, Int), acc: List[(Int, Int)]): (List[(Int, Int)], Int) =
      (indexValueSource, acc) match {
        case ((index, value), (indexCheck, valueCheck) :: tail) =>
          if (valueCheck > value) ((index, value) :: acc, indexCheck - index)
          else helper(indexValueSource, tail)
        case ((index, value), Nil) => ((index, value) :: Nil, 0)
      }

    temperatures.indices.zip(temperatures).foldRight((List.empty[(Int, Int)], List.empty[Int])) {
      case (indexValue, (checkList, acc)) =>
        val (newCheckList, result) = helper(indexValue, checkList)
        (newCheckList, result :: acc)
    }._2
  }

  println(calculate(example))
}
