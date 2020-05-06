package hackerrank.tasks.fp.functionalstructures.convexhull.ostap

import scala.math.sqrt

object Orientation extends Enumeration {
  val Clockwise, CounterClockwise, Linear = Value
}

case class Point(x: Int, y: Int)

object Solution {

  val delimiter = " "

  def main(args: Array[String]) {
    val pointsNum = scala.io.StdIn.readLine().toInt
    val pointsArr: IndexedSeq[Array[Int]] = (1 to pointsNum).map(
      _ =>
        (
          scala.io.StdIn
            .readLine()
            .split(delimiter)
            .map(_.trim.toInt)
          )
    )
    val pointsList = pointsArr.toList
    val startPoint = findStartPoint(pointsArr.toList)
    val z = findPath(pointsList, startPoint._1, startPoint._2)
    println(z)
  }

  def findStartPoint(points: List[Array[Int]]): (Point, Int) = {
    @scala.annotation.tailrec
    def findLoop(
                  pointsList: List[Array[Int]],
                  point: Array[Int] = Array(),
                  minX: Int = 10001,
                  minY: Int = 10001,
                  pos: Int = 0,
                  startPos: Int = 0
                ): (Point, Int) = { //вопрос к Никите по возвращению таплов? всегда ли плохо
      pointsList match {
        case Nil => (Point(point(0), point(1)), startPos)
        case head :: tail =>
          if ((head(0) < minX) || ((head(0) == minX) && (head(1) < minY))) {
            findLoop(tail, head, head(0), head(1), pos + 1, pos)
          } else {
            findLoop(tail, point, minX, minY, pos + 1, startPos)
          }
      }
    }
    findLoop(points)
  }

  def calcOrientation(point1: Point,
                      point2: Point,
                      point3: Point): Orientation.Value = {
    val orientation = (point2.y - point1.y) * (point3.x - point2.x) - (point2.x - point1.x) * (point3.y - point2.y)
    if (orientation == 0) {
      Orientation.Linear
    } else {
      if (orientation > 0) {
        Orientation.Clockwise
      } else {
        Orientation.CounterClockwise
      }
    }
  }

  def findPath(allPoints: List[Array[Int]],
               start: Point,
               startPos: Int): Double = {
    @scala.annotation.tailrec
    def pathLoop(points: List[Array[Int]],
                 point: Point,
                 res: List[Point] = List(),
                 perimeter: Double = 0): Double = {
      points match {
        case Nil => pathLoop(allPoints, point, res, perimeter) //next loop
        case head :: tail =>
          if (point == start) {
            if (res.size < 2) { //iteration start
              val nextPoint =
                findWisest(allPoints, point, Point(head(0), head(1)))
              pathLoop(
                tail,
                nextPoint,
                res.appended(nextPoint),
                calcPerimeter(perimeter, point, nextPoint)
              )
            } else {
              calcPerimeter(perimeter, point, start) //head find tail
            }
          } else {
            val nextPoint =
              findWisest(allPoints, point, Point(head(0), head(1)))
            pathLoop(
              tail,
              nextPoint,
              res.appended(nextPoint),
              calcPerimeter(perimeter, point, nextPoint)
            )
          }
      }
    }

    @scala.annotation.tailrec
    def findWisest(pointsToFindWisest: List[Array[Int]],
                   point1: Point,
                   point3: Point): Point = {
      pointsToFindWisest match {
        case Nil => point3
        case head :: tail =>
          if (calcOrientation(point1, Point(head(0), head(1)), point3) == Orientation.Clockwise) {
            findWisest(tail, point1, Point(head(0), head(1)))
          } else {
            findWisest(tail, point1, point3)
          }
      }
    }

    def calcPerimeter(perimeter: Double, point1: Point, point2: Point) = {
      perimeter + (sqrt(
        (scala.math.pow(point1.x - point2.x, 2)) + scala.math
          .pow(point1.y - point2.y, 2)
      ))
    }
    pathLoop(allPoints.takeRight(startPos), start)
  }
}
