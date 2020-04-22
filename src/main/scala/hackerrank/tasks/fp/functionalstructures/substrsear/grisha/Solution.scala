package hackerrank.tasks.fp.functionalstructures.substrsear.grisha

import scala.io.StdIn.{readLine, readInt}

object Solution {
  def kmp_search3(text: String, pattern: String): Boolean = {
    val checkerString = pattern + "@" + text
    val length = checkerString.length
    val array: Array[Int] = new Array(length)

    @scala.annotation.tailrec
    def innerFindPrefix(start: Int, end: Int, acc: Int = 0): Array[Int] = {
      if (end == length) array
      else if (checkerString(start) == checkerString(end)) {
        array(end) = acc + 1
        innerFindPrefix(start + 1, end + 1, acc + 1)
      } else if (start == 0) {
        array(end) = 0
        innerFindPrefix(start - acc, end + 1)
      } else {
        innerFindPrefix(array(start - 1), end, array(start - 1))
      }
    }
    pattern.length <= text.length &&
    innerFindPrefix(0, 1).contains(pattern.length)
  }

  def main(args: Array[String]) {
    val n = readInt()
    for (i <- 1 to n) {
      val result = kmp_search3(readLine(), readLine())
      if (result) println("YES") else println("NO")
    }
  }
}
