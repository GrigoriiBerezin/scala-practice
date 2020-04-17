package hackerrank.tasks.fp.functionalstructures.substrsear.nikitam

import scala.io.StdIn.{readInt, readLine}

object Solution {
  def main(args: Array[String]) {

    val amountOfTasks = readInt()
    val textAndWord = List.fill(amountOfTasks)((readLine, readLine))

    val answer =
      textAndWord
        .map((doTheThing _).tupled)
        .map(if (_) "YES" else "NO")
        .mkString(System.lineSeparator)

    println(answer)

  }

  def doTheThing(text: String, word: String): Boolean = {

    val prefixTable = getPrefixTable(word)

    @scala.annotation.tailrec
    def go(i: Int, j: Int): Boolean = {
      if (j == word.length) true
      else if (i == text.length) false
      else if (text(i) == word(j)) go(i + 1, j + 1)
      else if (j == 0) go(i + 1, j)
      else if (prefixTable(j - 1) != 0) go(i, prefixTable(j - 1))
      else go(i, 0)
    }

    go(0, 0)
  }

  def getPrefixTable(word: String): Vector[Int] = {

    @scala.annotation.tailrec
    def goBack(i: Int, j: Int, word: String, list: Vector[Int]): Int = {
      if (j == 0 || word(i) == word(j)) j
      else goBack(i, list(j - 1), word, list)
    }

    @scala.annotation.tailrec
    def go(i: Int, j: Int, acc: Vector[Int]): Vector[Int] = {
      lazy val updatedJ = goBack(i, j, word, acc)
      if (i == word.length) acc
      else if (word(i) == word(updatedJ))
        go(i + 1, updatedJ + 1, acc :+ (updatedJ + 1))
      else
        go(i + 1, updatedJ, acc :+ 0)
    }
    go(1, 0 , Vector(0))
  }
}
