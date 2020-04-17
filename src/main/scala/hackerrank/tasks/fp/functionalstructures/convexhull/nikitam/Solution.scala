package hackerrank.tasks.fp.functionalstructures.convexhull.nikitam

import scala.io.StdIn.{readInt, readLine}

object Solution {

  private val ordByHeight: Ordering[Point] = Ordering.by[Point, Int](_.y)
  private val toOrdByAngle: Point => Ordering[Point] = basePoint => Ordering.by[Point, Double] { p =>
    val sin = (p.y - basePoint.y) / findDistance(p, basePoint)
    val angle = Math.asin(sin)
    if (p.x < basePoint.x) Math.PI - angle else angle
  }
  private val toOrdByDistance: Point => Ordering[Point] = basePoint => Ordering.by[Point, Double](findDistance(_, basePoint))

  def main(args: Array[String]): Unit = {
    val amountOfPoints = readInt
    val coordinates = List.fill(amountOfPoints)(readLine)

    val points = coordinates.map(parsePoint)
    val convexHull = findConvexHull(points)
    val perimeter = findPerimeter(convexHull)

    println(perimeter)
  }

  def findPerimeter(points: List[Point]): Double = {
    @scala.annotation.tailrec
    def makeLines(firstPoint: Point, points: List[Point], acc: List[Line] = Nil): List[Line] =
      points match {
        case from :: to :: _ => makeLines(firstPoint, points.tail, Line(from, to) :: acc)
        case lastPoint :: _ => Line(lastPoint, firstPoint) :: acc
        case _ => acc
      }

    makeLines(points.head, points).map(findLength).sum
  }

  def findConvexHull(points: List[Point]): List[Point] = {
    val basePoint = points.min(ordByHeight)
    val ordByAngle = toOrdByAngle(basePoint)
    val ordByDistance = toOrdByDistance(basePoint)

    val sortedPoints = points.sorted(ordByAngle.orElse(ordByDistance))
    findConvexHullOfSortedPoints(sortedPoints)
  }

  private def findConvexHullOfSortedPoints(points: List[Point]): List[Point] = {

    def isLeftTurn(line: Line, candidate: Point): Boolean = {
      val lineLength = findLength(line)

      val Line(from, to) = line
      val cos = (to.x - from.x) / lineLength
      val sin = (to.y - from.y) / lineLength

      val rotatedY = (candidate.y - from.y) * cos - (candidate.x - from.x) * sin

      rotatedY kindaGt 0
    }

    @scala.annotation.tailrec
    def combine(currPath: List[Point], candidate: Point): List[Point] = currPath match {
      case to :: from :: _ if isLeftTurn(Line(from, to), candidate) => candidate :: currPath
      case _ :: tail if tail.nonEmpty => combine(tail, candidate)
      case _ => candidate :: currPath
    }

    points.foldLeft(List.empty[Point])((currPath, point) => combine(currPath, point))
  }

  private def findDistance(from: Point, to: Point): Double =
    Math.sqrt(Math.pow(from.x - to.x, 2) + Math.pow(from.y - to.y, 2))

  private def findLength(line: Line): Double =
    findDistance(line.from, line.to)

  private def parsePoint(input: String): Point = {
    val Array(x, y) = input.split(" ").map(_.toInt)
    Point(x, y)
  }

  case class Point(x: Int, y: Int)

  case class Line(from: Point, to: Point)

  implicit class ComparableDouble(val value: Double) {
    private val defaultPrecision = 1E-10

    def kindaEqual(that: Double, precision: Double = defaultPrecision): Boolean =
      (this.value - that).abs < precision

    def kindaGt(that: Double, precision: Double = defaultPrecision): Boolean =
      !this.kindaEqual(that, precision) && this.value > that.value
  }

}


