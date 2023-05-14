package hackerrank.tasks.fp

import scala.annotation.tailrec
import scala.io.StdIn

object PasswordCracker extends App {

  def resolveCrack(passwords: List[String], attempt: String): Option[String] = {
    @tailrec def inner(leftAttempt: String, acc: List[String]): List[String] =
      passwords.filter(pass => leftAttempt.startsWith(pass)) match {
        case h :: _ => inner(leftAttempt.replaceFirst(h, ""), h :: acc)
        case Nil if leftAttempt.isEmpty => acc
        case Nil => List.empty
      }

    inner(attempt, List.empty[String]) match {
      case Nil => None
      case l => Some(l.reverse.mkString(" "))
    }
  }

  def resolveCrackAll(passwords: List[String], attempt: String): Option[String] = {
    def inner(leftAttempt: String, acc: List[List[String]]): List[List[String]] =
      if (leftAttempt.isEmpty) acc
      else passwords.filter(pass => leftAttempt.startsWith(pass)).flatMap { pass =>
        val newResults = acc match {
          case head :: tail => (pass :: head) :: tail
          case Nil => List(List(pass))
        }
        inner(leftAttempt.replaceFirst(pass, ""), newResults)
      }

    inner(attempt, List.empty) match {
      case head :: _ => Some(head.reverse.mkString(" "))
      case Nil => None
    }
  }

  val t = StdIn.readInt()

  val testCases = (1 to t).map { _ =>
    val _ = StdIn.readInt()
    val passwords = StdIn.readLine().split("\\s+").toList
    val attempt = StdIn.readLine()
    (passwords, attempt)
  }.toList

  val result = testCases.map { case (passwords, attempt) =>
    resolveCrackAll(passwords, attempt).getOrElse("WRONG PASSWORD")
  }

  val outputResult = result.mkString(System.lineSeparator())
  println(outputResult)
}
