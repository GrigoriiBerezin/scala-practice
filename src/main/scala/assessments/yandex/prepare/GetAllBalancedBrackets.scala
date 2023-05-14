package assessments.yandex.prepare

import scala.io.StdIn.readInt

object GetAllBalancedBrackets extends App {

  def calculate(bracketCount: Int): Unit = {
    def inner(openCount: Int, total: Int, acc: String): Unit =
      if (openCount == 0) if (total == bracketCount) println(acc) else inner(1, total, acc + "(")
      else
        if (openCount + total == bracketCount)
          println(acc + Range(0, openCount).foldLeft("")((acc, _) => acc + ")"))
        else {
          inner(openCount + 1, total, acc + "(")
          inner(openCount - 1, total + 1, acc + ")")
        }

    inner(0, 0, "")
  }

  calculate(readInt)
}
