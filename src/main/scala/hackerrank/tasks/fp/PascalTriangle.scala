package hackerrank.tasks.fp

object PascalTriangle extends App {

  class PascalTriangle() {

    def showFull(n: Int): Unit = {
      val triangleToShow = getFull(n)
      val lineSize = getRow(n).mkString(" ").length
      triangleToShow.map(l => centerString(lineSize, l.mkString(" "))).foreach(println)
    }

    def getFull(n: Int): List[List[Int]] = pascalTriangle.take(n).toList

    def showRow(n: Int): Unit = println(getRow(n).mkString(" "))

    def getRow(n: Int): List[Int] = pascalTriangle(n - 1)

    def showCell(x: Int, y: Int): Unit =
      println(getCell(x, y).getOrElse("No such element in a row!"))

    def getCell(x: Int, y: Int): Option[Int] = getRow(x).lift(y - 1)

    private def centerString(width: Int, s: String): String = String
      .format("%-" + width + "s", String.format("%" + (s.length + (width - s.length) / 2) + "s", s))

    private val pascalTriangle: LazyList[List[Int]] = {
      def loop(l: List[Int]): LazyList[List[Int]] = {
        val nextRow = 0 :: l.sliding(2).map(_.sum).foldRight(List(0))((v, acc) => v :: acc)
        l #:: loop(nextRow)
      }

      List(0) #:: loop(List(0, 1, 0))
    }

  }

  private val triangle = new PascalTriangle
  triangle.showFull(10)
  triangle.showRow(10)
  triangle.showCell(10, 6)
}
