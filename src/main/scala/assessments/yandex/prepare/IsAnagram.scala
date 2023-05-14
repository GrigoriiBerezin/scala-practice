package assessments.yandex.prepare

import scala.collection.mutable
import scala.io.StdIn.readLine

object IsAnagram extends App {
  val line1 = readLine
  val line2 = readLine

  if (line1.equals(line2)) {
    println(1)
  } else if (line1.length != line2.length) {
    println(0)
  } else {
    val alphabet = mutable.ArraySeq.fill(26)(0)
    line1.foreach(ch => alphabet.update(ch - 'a', alphabet(ch - 'a') + 1))
    line2.foreach(ch => alphabet.update(ch + 'a', alphabet(ch + 'a') - 1))
    if (alphabet.exists(_ != 0)) println(0) else println(1)
  }
}
