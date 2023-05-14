package hackerrank.tasks.fp.recursion.sequenceOfFullColors.grisha

import scala.annotation.tailrec
import scala.io.StdIn
import cats.implicits._

object SequenceFullOfColors extends App {

  def isFullOfColors(test: String): Boolean = {
    @tailrec def inner(restColors: List[Char], buffer: Map[Char, Int] = Map.empty): Boolean =
      restColors match {
        case Nil => buffer.getOrElse('R', 0) == buffer.getOrElse('G', 0) &&
          buffer.getOrElse('Y', 0) == buffer.getOrElse('B', 0)
        case head :: tail =>
          val newBuffer = buffer |+| Map(head -> 1)
          if (
            Math.abs(newBuffer.getOrElse('R', 0) - Math.abs(newBuffer.getOrElse('G', 0))) > 1 ||
            Math.abs(newBuffer.getOrElse('Y', 0) - Math.abs(newBuffer.getOrElse('B', 0))) > 1
          ) false
          else inner(tail, newBuffer)
      }

    inner(test.toList)
  }

  val i = StdIn.readInt()
  val tests = (1 to i).map(_ => StdIn.readLine().trim)
  val results = tests.map(isFullOfColors)
  val validOutput = results.map(_.toString.capitalize).mkString(System.lineSeparator())
  println(validOutput)
}
