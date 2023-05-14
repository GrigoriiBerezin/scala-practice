package hackerrank.tasks.fp.recursion.stringOPermute.grisha

import scala.io.StdIn

object StringOPermute extends App {
  val count = StdIn.readInt()
  val lines = (1 to count).map(_=> StdIn.readLine())
  val reversedLines = lines.map(line => line.grouped(2).map(_.reverse).mkString(""))
  val validOutput = reversedLines.mkString("\n")
  println(validOutput)
}
