package hackerrank.tasks.fp.algorithms.erato.grisha

import scala.annotation.tailrec

object Erato extends App {
  val n = 1000

  def erato(n: Int): Seq[Int] = {
    @tailrec
    def inner(from: Seq[Int], p: Int, acc: Seq[Int]): Seq[Int] = {
      val newFrom = from.filter(v => v % p != 0)
      val newPOpt = newFrom.find(v => v > p)
      newPOpt match {
        case Some(newP) => inner(newFrom, newP, p +: acc)
        case None => p +: acc
      }
    }
    inner(2 to n, 2, Seq.empty[Int])
  }

  def firstPrimes(n: Int): Seq[Int] = {
    def isPrime(toCheck: Int, delims: Seq[Int]): Boolean = {
      delims.forall(v => toCheck % v != 0)
    }

    @tailrec
    def inner(i: Int, toCheck: Int, delims: Seq[Int]): Seq[Int] = {
      if (i >= n) delims
      else {
        if (isPrime(toCheck, delims)) inner(i + 1, toCheck + 1, toCheck +: delims)
        else inner(i, toCheck + 1, delims)
      }
    }

    inner(0, 2, Seq.empty[Int])
  }

  println(erato(n))
  println(firstPrimes(n))
}
