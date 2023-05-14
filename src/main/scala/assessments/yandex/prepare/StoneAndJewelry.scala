package assessments.yandex.prepare

import scala.io.StdIn.readLine

object StoneAndJewelry extends App {
  val j = readLine().toCharArray.toSet
  val s = readLine().toCharArray.toList

  println(s.foldLeft(0)((acc, ch) => if (j.contains(ch)) acc + 1 else acc))
}
