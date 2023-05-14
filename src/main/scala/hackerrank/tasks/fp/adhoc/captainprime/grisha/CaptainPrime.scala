package hackerrank.tasks.fp.adhoc.captainprime.grisha

import scala.annotation.tailrec
import scala.io.StdIn

object CaptainPrime extends App {

  sealed abstract class Position(val value: String) {
    override def toString: String = value
  }

  object Position {

    case object Left extends Position("LEFT")
    case object Central extends Position("CENTRAL")
    case object Right extends Position("RIGHT")
    case object Dead extends Position("DEAD")

    def apply(id: Int): Position =
      if (isPrime(id) && !containsZero(id)) (isLeft(id), isRight(id)) match {
        case (true, true) => Central
        case (true, false) => Right
        case (false, true) => Left
        case (false, false) => Dead
      }
      else Dead

    private def isPrime(id: Int): Boolean = {
      @tailrec def inner(delim: Int): Boolean = delim == id || (id % delim != 0 && inner(delim + 1))

      if (id == 1) false else inner(2)
    }

    private def containsZero(id: Int): Boolean = id.toString.contains('0')

    private def isLeft(id: Int): Boolean = toScan(id)(_.scanLeft("")((acc, digit) => s"$acc$digit"))

    private def isRight(id: Int): Boolean =
      toScan(id)(_.scanRight("")((digit, acc) => s"$digit$acc"))

    private def toScan(id: Int)(scan: Seq[Char] => Seq[String]): Boolean = scan(id.toString)
      .filter(v => v.nonEmpty && v != id.toString).forall(v => isPrime(v.toInt))

  }

  val n = StdIn.readInt()
  val digits = (1 to n).map(_ => StdIn.readInt())
  val result = digits.map(Position(_)).mkString(System.lineSeparator())
  println(result)
}
