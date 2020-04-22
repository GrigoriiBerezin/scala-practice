package hackerrank.tasks.fp.functionalstructures.convexhull.grisha

import scala.io.StdIn.{readInt, readLine}

object Solution {
  def mcs(points: List[(Int, Int)]): List[(Int, Int)] = {
    def rotate(startPoint: (Int, Int),
               endPoint: (Int, Int),
               point: (Int, Int)): Int = {
      def vectorCoord(startPoint: (Int, Int),
                      endPoint: (Int, Int)): (Int, Int) =
        (endPoint._1 - startPoint._1, endPoint._2 - startPoint._2)

      val (vectorX, vectorY) = vectorCoord(startPoint, endPoint)
      val (pointX, pointY) = vectorCoord(startPoint, point)

      vectorX * pointY - vectorY * pointX
    }

    @scala.annotation.tailrec
    def findPeeks(points: List[(Int, Int)],
                  peeks: List[(Int, Int)]): List[(Int, Int)] = {
      points match {
        case x :: xs =>
          if (rotate(peeks.head, peeks.tail.head, x) < 0)
            findPeeks(points, peeks.tail)
          else findPeeks(xs, x :: peeks)
        case Nil => peeks
      }
    }

    points match {
      case _ :: _ :: _ =>
        val sortedPoints =
          points.sortWith(_._2 < _._2).sortWith(_._1 < _._1)
        val startPoint = sortedPoints.head
        val lastPoints =
          sortedPoints.tail.sorted((p1, p2) => rotate(startPoint, p1, p2))
        findPeeks(lastPoints.tail, List(lastPoints.head, startPoint))
      case _ => List()
    }
  }

  def perimeter(peeks: List[(Int, Int)]): Double = {
    def dist(p1: (Int, Int), p2: (Int, Int)): Double =
      math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))

    val lines = peeks zip (peeks.tail ++ List(peeks.head))
    lines.map(line => dist(line._1, line._2)).sum
  }

  def read(): List[(Int, Int)] =
    (1 to readInt())
      .map(_ => {
        val line = readLine().split("\\s").toList
        (line.head.toInt, line.last.toInt)
      })
      .toList

  def main(args: Array[String]): Unit = {
    println(perimeter(mcs(read())))
  }
}
