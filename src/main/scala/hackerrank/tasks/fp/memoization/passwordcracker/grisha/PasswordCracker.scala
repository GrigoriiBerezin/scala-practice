package hackerrank.tasks.fp.memoization.passwordcracker.grisha

import scala.io.StdIn

object PasswordCracker extends App {
  def crackPassword(passwords: Seq[String], loginAttempt: String): String = ???

  private val t = StdIn.readInt()

  private val cases = (1 to t).map { _ =>
    val _ = StdIn.readInt()
    val passwords = StdIn.readLine().split(" ")
    val loginAttempt = StdIn.readLine()
    (passwords, loginAttempt)
  }

  private val answers = cases.map { case (passwords, loginAttempt) =>
    crackPassword(passwords, loginAttempt)
  }
  private val validOutput = answers.mkString(System.lineSeparator())
  println(validOutput)
}
