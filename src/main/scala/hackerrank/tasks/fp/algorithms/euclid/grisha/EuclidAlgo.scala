package hackerrank.tasks.fp.algorithms.euclid.grisha

import zio._

import scala.annotation.tailrec
import scala.io.StdIn

object EuclidAlgo extends ZIOAppDefault {

  val euclid: (Int, Int) => Int = { (m, n) =>
    @tailrec
    def inner(m: Int, n: Int): Int = {
      val r = m % n
      if (r == 0) n
      else inner(n, r)
    }

    if (m > n) inner(m, n)
    else inner(n, m)
  }

  val main: ZIO[Any, Throwable, Unit] = for {
    m <- ZIO.attempt(StdIn.readInt())
    n <- ZIO.attempt(StdIn.readInt())
    divider <- ZIO.attempt(euclid(m, n))
    _ <- ZIO.attempt(println(divider))
  } yield ()

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = main.exitCode
}
