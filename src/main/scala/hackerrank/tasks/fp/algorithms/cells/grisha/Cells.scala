package hackerrank.tasks.fp.algorithms.cells.grisha

import scala.io.StdIn

object Cells extends App {
  def getLargestArea(rows: Int, cols: Int, matrix: Array[Array[Int]]): Int = {
    def findLargestArea(row: Int, col: Int, matrix: Array[Array[Int]]): Int = {
      if (row < 0 || row >= rows || col < 0 || col >= cols || matrix(row)(col) == 0) 0
      else {
        val updatedMatrix = matrix.updated(row, matrix(row).updated(col, 0))

        (for {
          r <- row - 1 to row + 1
          c <- col - 1 to col + 1
        } yield if (r != c) findLargestArea(r, c, updatedMatrix) else 0).sum + 1
      }
    }

    (for {
      row <- 0 until rows
      col <- 0 until cols
    } yield {
      if (matrix(row)(col) == 1) findLargestArea(row, col, matrix) else 0
    }).max
  }

  val n = StdIn.readLine().toInt
  val m = StdIn.readLine().toInt

  val matrix = (1 to n).toArray
    .map(_ => StdIn.readLine().split(" ").map(_.toInt))

  println(getLargestArea(n, m, matrix))
}
