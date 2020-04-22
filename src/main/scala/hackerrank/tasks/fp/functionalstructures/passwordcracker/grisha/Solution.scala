package hackerrank.tasks.fp.functionalstructures.passwordcracker.grisha

import scala.io.StdIn.{readInt, readLine}

object Solution {
  def passwordCracker(passwords: Array[String], attempt: String): Unit = {
    val matching = passwords.mkString("|").r
    val suggestPassword = matching.findAllIn(attempt).mkString(" ")
    if (attempt.length == suggestPassword.replace(" ", "").length)
      println(suggestPassword)
    else println("WRONG PASSWORD")
  }

  def main(args: Array[String]) {
    for (_ <- readInt() until 0 by -1) {
      val _ = readInt()
      val passwords = readLine().trim.split(" ")
      val attempt = readLine().trim()

      passwordCracker(passwords, attempt)
    }
  }
}
