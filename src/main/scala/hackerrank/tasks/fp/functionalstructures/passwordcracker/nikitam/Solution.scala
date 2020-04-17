package hackerrank.tasks.fp.functionalstructures.passwordcracker.nikitam

import java.io.PrintWriter

import scala.io.StdIn
import scala.util.Using

object Cracker {

  def passwordCracker(passwords: Array[String], loginAttempt: String): String = {
    val pwds = removeRedundancy(passwords.toList)
    findPass(pwds, loginAttempt).map(_.mkString(" ")).getOrElse("WRONG PASSWORD")
  }

  private def removeRedundancy(words: Passwords): Passwords = {
    val sorted = words.sortBy(_.length)

    @scala.annotation.tailrec
    def inner(w: Passwords, acc: Passwords): Passwords = w match {
      case Nil => acc
      case head :: tail =>
        if (findPass(acc, head).isDefined) inner(tail, acc)
        else inner(tail, head +: acc)
    }

    inner(sorted, Nil)
  }

  type Passwords = List[String]

  private def findPass(passwords: Passwords, login: String): Option[Passwords] = {

    val maxRollback: Int = {
      val minPassLength = passwords.minByOption(_.length).map(_.length)
      val maxPassLength = passwords.maxByOption(_.length).map(_.length)
      minPassLength.flatMap(min => maxPassLength.map(max => max / min + 1)).getOrElse(0)
    }

    def upToMaxRollback(n: Int): Int = upTo(maxRollback)(n)

    def findPassWithAcc(currPass: Passwords, login: String, acc: Passwords, rollBackCnt: Int): Either[Int, Passwords] =
      if (login.isEmpty) Right(acc.reverse)
      else if (rollBackCnt <= 0) Left(rollBackCnt)
      else currPass match {
        case head :: tail if login.startsWith(head) =>

          val loginRemainder = login.drop(head.length)
          val accWithPass = head +: acc
          val updatedRollBackCnt = upToMaxRollback(rollBackCnt + 1)
          findPassWithAcc(passwords, loginRemainder, accWithPass, updatedRollBackCnt)
            .recoverWith(cnt => findPassWithAcc(tail, login, acc, cnt - 1))

        case _ :: tail =>
          findPassWithAcc(tail, login, acc, rollBackCnt)
        case _ =>
          Left(rollBackCnt - 1)
      }

    findPassWithAcc(passwords, login, Nil, maxRollback).toOption
  }

  private def upTo(max: Int)(n: Int): Int = if (n > max) max else n

  private implicit class EitherOps[L, R](either: Either[L, R]) {
    def recoverWith[LL >: L, RR >: R](f: L => Either[LL, RR]): Either[LL, RR] = either.fold(f, Right(_))
  }

}

object Solution {
  def main(args: Array[String]) {
    Using(new PrintWriter(sys.env("OUTPUT_PATH"))) { printWriter =>

      val t = StdIn.readInt

      for (_ <- 1 to t) {
        val _ = StdIn.readInt

        val passwords = StdIn.readLine.replaceAll("\\s+$", "").split(" ")
        val loginAttempt = StdIn.readLine

        val result = Cracker.passwordCracker(passwords, loginAttempt)

        printWriter.println(result)
      }
    }
  }
}
