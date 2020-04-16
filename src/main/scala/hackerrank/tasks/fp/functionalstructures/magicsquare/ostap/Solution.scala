package hackerrank.tasks.fp.functionalstructures.magicsquare.ostap

object Solution {

  // Complete the formingMagicSquare function below.
  def formingMagicSquare(s: Array[Array[Int]]): Int = {

    def magisizeQuad(s: Array[Array[Int]],
                     cost: Int): (Array[Array[Int]], Int) = {

      val columnNums = s(0).size
      val colRange: Range =
        0 to columnNums - 1

      val s_vertical = Array.ofDim[Int](columnNums, columnNums)

      for {
        x <- colRange
        y <- colRange
      } yield s_vertical(x)(y) = s(y)(x) //transform matrix

      val horisums: List[Int] = s.map(x => x.sum).toList
      val vertsums: List[Int] = s_vertical.map(x => x.sum).toList
      val sumsSet = horisums.concat(vertsums).toSet
      val sumsList = horisums.concat(vertsums)
      println(sumsSet)

      if (sumsSet.size == 1) { println(cost); (s, cost) } else {

        val minSum = sumsSet.min
        val maxSum = sumsSet.max
        println(minSum)
        println(maxSum)

        val normal = sumsList.groupBy(identity).maxBy(_._2.size)._1

        val upper = Math.abs(maxSum - normal)
        val lower = Math.abs(normal - minSum)

        val diff: (Int, Boolean) = if (upper >= lower) { //Boolean - isUpper
          (upper, true)
        } else {
          (lower, false)
        }
        val toReplace = if (diff._2) maxSum else minSum

        val hHas: Int = horisums.indexOf(toReplace)
        val coordToReplace: (Int, Boolean) = if (hHas == -1) {
          (vertsums.indexOf(toReplace), false)
        } else {
          (hHas, true)
        }

        if (coordToReplace._2) { //seacrhing what column to replace in this row
          if (diff._2) { //if diff is upper searching for other upper elements
            val verIndex = vertsums.indexOf(vertsums.max) //replacing the first match
            s(coordToReplace._1)(verIndex) = s(coordToReplace._1)(verIndex) - diff._1
          } else { //if diff is lower searching for other lower elements
            val verIndex = vertsums.indexOf(vertsums.min) //replacing the first match
            s(coordToReplace._1)(verIndex) = s(coordToReplace._1)(verIndex) + diff._1
          }
        } else {
          if (diff._2) {
            val horisIndex = horisums.indexOf(horisums.max)
            s(coordToReplace._1)(horisIndex) = s(coordToReplace._1)(horisIndex) - diff._1
          } else {
            val horisIndex = horisums.indexOf(horisums.min)
            s(coordToReplace._1)(horisIndex) = s(coordToReplace._1)(horisIndex) + diff._1
          }
        }
        magisizeQuad(s, cost + diff._1)
      }
    }

    val res = magisizeQuad(s, 0)
    res._2
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
