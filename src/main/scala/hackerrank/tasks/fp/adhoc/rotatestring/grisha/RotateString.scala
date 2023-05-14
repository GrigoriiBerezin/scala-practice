package hackerrank.tasks.fp.adhoc.rotatestring.grisha

import scala.io.StdIn

object RotateString extends App {

  def rotateString(str: String): Seq[String] = {
    def view(i: Int): String = str.drop(i) ++ str.take(i)

    (1 to str.length).map(view)
  }

  val n = StdIn.readInt()
  val strings = (1 to n).map(_ => StdIn.readLine())
  val results = strings.map(rotateString)
  val validOutput = results.map(_.mkString(" ")).mkString(System.lineSeparator())
  println(validOutput)
}
