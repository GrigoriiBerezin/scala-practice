package assessments.youtube

import scala.annotation.tailrec

object FindValueInMatrix extends App {

  val matrix = Vector(
    Vector(1, 4, 7, 11, 15, 16),
    Vector(2, 5, 8, 12, 19, 22),
    Vector(3, 6, 9, 14, 22, 24),
    Vector(10, 13, 16, 17, 24, 27),
    Vector(18, 21, 23, 26, 30, 36)
  )

  def isInMatrix(matrix: Vector[Vector[Int]], toFindValue: Int): Boolean = {
    @tailrec def inner(x: Int, y: Int): Boolean = matrix.length > y && matrix(y).length > x && x > 0 && {
      val value = matrix(y)(x)
      if (value > toFindValue) inner(x - 1, y)
      else if (value < toFindValue) inner(x, y + 1)
      else true
    }

    matrix.nonEmpty && matrix(0).nonEmpty && inner(matrix(0).length - 1, 0)
  }

  println(isInMatrix(matrix, -2))

}
