package hackerrank.tasks.fp.algorithms.encryption.grisha

import scala.io.StdIn

object Encryption extends App {
  var t = StdIn.readLine()
  t = t.split(" ").map(_.trim).mkString("")

  var s = Math.sqrt(t.size)
  var is = s.toInt
  var r = is
  var c = if (is == s) is else (s + 1).toInt
  r = if (r * c < t.size) r + 1 else r

  var m = t.grouped(c).toList
  var v = for {
    x <- 0 until c
    y <- 0 to r
  } yield {
    var sp = if ((y + 1) % r == 0) " " else ""
    try {
      m(y)(x) + sp
    } catch {
      case _: Exception => "" + sp
    }
  }
  var a = v.mkString("")
  print(a)
}
