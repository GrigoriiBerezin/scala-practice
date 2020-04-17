package hackerrank.tasks.fp.functionalstructures.validbst.nikitam

import scala.io.StdIn

object Solution {

  def main(args: Array[String]): Unit = {
    val t = StdIn.readInt()
    val inputLines = List.fill(t) {
      val _ = StdIn.readInt()
      StdIn.readLine()
    }

    val input = inputLines.map(_.split(" ").map(_.toInt).toList)
    val solutions = input.map(solve).map(if (_) "YES" else "NO")
    val answer = solutions.mkString(System.lineSeparator)

    println(answer)
  }

  def solve(list: List[Int]): Boolean = {
    list match {
      case Nil => true
      case root :: tree =>
        val (left, right) = tree.span(_ < root)
        right.forall(_ > root) && solve(left) && solve(right)
    }
  }
}
