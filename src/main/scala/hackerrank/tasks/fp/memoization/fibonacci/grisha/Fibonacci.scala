package hackerrank.tasks.fp.memoization.fibonacci.grisha

import scala.io.StdIn

object Fibonacci extends App {

  lazy val fib: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fib.zip(fib.tail).map { case (a, b) =>
    a + b
  }

  private val count = StdIn.readInt()
  private val indexes = (1 to count).map(_ => StdIn.readInt())
  private val fibs = indexes.map(i => fib(i) % (Math.pow(10, 8) + 7).toInt)
  private val validOutput = fibs.mkString(System.lineSeparator())
  println(validOutput)
}
