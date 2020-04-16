package hackerrank.tasks.fp.functionalstructures.passwordcracker.ostap

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
//import scala.collection.parallel.immutable._
//import scala.collection.parallel.mutable._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._

object Result {

  /*
   * Complete the 'passwordCracker' function below.
   *
   * The function is expected to return a STRING.
   * The function accepts following parameters:
   *  1. STRING_ARRAY passwords
   *  2. STRING loginAttempt
   */

  def passwordCracker(passwords: Array[String],
                      loginAttempt: String): String = {
    // Write your code here
    ???
  }

}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
    val t = StdIn.readLine.trim.toInt
    for (tItr <- 1 to t) {
      val n = StdIn.readLine.trim.toInt
      val passwords = StdIn.readLine.replaceAll("\\s+$", "").split(" ")
      val loginAttempt = StdIn.readLine
      println(passwords)
      println(loginAttempt)
//      val result = Result.passwordCracker(passwords, loginAttempt)
//      printWriter.println(result)
    }
    printWriter.close()
  }
}n(result)
    }
    printWriter.close()
  }
}
