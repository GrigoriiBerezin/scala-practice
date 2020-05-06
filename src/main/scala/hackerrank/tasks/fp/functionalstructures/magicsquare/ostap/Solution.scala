package hackerrank.tasks.fp.functionalstructures.magicsquare.ostap

import java.io._
import java.math._
import java.security._
import java.text._
//import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Solution {

  // Complete the formingMagicSquare function below.
  def formingMagicSquare(s: Array[Array[Int]]): Int = {


    val columnNums = 3
    val colRange: Range =
      0 to columnNums - 1

    val value = List(
      Array(List(2, 7, 6),List(9, 5, 1),List(4, 3, 8)),
      Array(List(2, 9, 4),List(7, 5, 3),List(6, 1, 8)),
      Array(List(4, 3, 8),List(9, 5, 1),List(2, 7, 6)),
      Array(List(4, 9, 2),List(3, 5, 7),List(8, 1, 6)),
      Array(List(6, 1, 8),List(7, 5, 3),List(2, 9, 4)),
      Array(List(6, 7, 2),List(1, 5, 9),List(8, 3, 4)),
      Array(List(8, 1, 6),List(3, 5, 7),List(4, 9, 2)),
      Array(List(8, 3, 4),List(1, 5, 9),List(6, 7, 2))
    )

    def isMagic(s: Array[List[Int]]): Boolean = {

      val s_vertical = Array.ofDim[Int](columnNums, columnNums)

      for {
        x <- colRange
        y <- colRange
      } yield s_vertical(x)(y) = s(y)(x) //transform matrix

      val horisums: Set[Int] = s.map(x => x.sum).toSet
      val vertsums: Set[Int] = s_vertical.map(x => x.sum).toSet
      val sumsSet = horisums.concat(vertsums)

      if ((sumsSet.size == 1) && sumsSet.contains(15)) {
        true
      } else {
        false
      }
    }

    val magics: List[Array[List[Int]]] = for {
      list <- value
      if isMagic(list)
    } yield list

    def getCost(arr: Array[List[Int]]): Int = {
      val costs = for {
        x <- colRange
        y <- colRange
      } yield Math.abs(arr(x)(y) - s(x)(y))
      costs.sum
    }

    val res: List[Int] = for {
      arr <- magics
    } yield getCost(arr)
    res.min
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val s = Array.ofDim[Int](3, 3)

    for (i <- 0 until 3) {
      s(i) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    val result = formingMagicSquare(s)

    printWriter.println(result)

    printWriter.close()
  }
}

