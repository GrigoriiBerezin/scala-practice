package hackerrank.tasks.fp

import scala.annotation.tailrec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IteratorSuite extends AnyFlatSpec with Matchers {

  private val fib = Iterator.fib
  private val odd = Iterator.odd
  private val sort = Iterator.sort(Seq(1, 8, 5))
  private val from = Iterator.from("A", "B", "C")

  behavior of "next"

  it should "get first 10 fib numbers" in {
    val expected = Seq(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)

    val (actual, _) = getFirst(fib, 10)

    actual shouldBe expected
  }

  it should "get first 10 odd numbers" in {
    val expected = Seq(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)

    val (actual, _) = getFirst(odd, 10)

    actual shouldBe expected
  }

  it should "get first 3 sort elements" in {
    val expected = Seq(1, 5, 8)

    val (actual, _) = getFirst(sort, 3)

    actual shouldBe expected
  }

  it should "get first 3 from elements" in {
    val expected = Seq("A", "B", "C")

    val (actual, _) = getFirst(from, 3)

    actual shouldBe expected
  }

  it should "throw an error if no elements left" in {
    val (_, newSort) = getFirst(sort, 3)

    assertThrows[UnsupportedOperationException](newSort.next())
  }

  behavior of "hasNext"

  it should "always be true for fib" in {
    val (_, newFib) = getFirst(fib, 15)

    fib.hasNext shouldBe true
    newFib.hasNext shouldBe true
  }

  it should "always be true for odd" in {
    val (_, newOdd) = getFirst(odd, 15)

    odd.hasNext shouldBe true
    newOdd.hasNext shouldBe true
  }

  it should "be true only if has next element" in {
    val (_, newSort) = getFirst(sort, 3)

    sort.hasNext shouldBe true
    newSort.hasNext shouldBe false
  }

  behavior of "peek"

  it should "return next element without iterating" in {
    val first = sort.peek()
    val firstAgain = sort.peek()

    first shouldBe firstAgain
  }

  private def getFirst[A](iterator: Iterator[A], i: Int): (Seq[A], Iterator[A]) = {
    @tailrec def inner(i: Int, iterator: Iterator[A], acc: Seq[A]): (Seq[A], Iterator[A]) =
      if (i == 0) (acc, iterator)
      else {
        val (result, newIterator) = iterator.next()
        inner(i - 1, newIterator, acc :+ result)
      }

    inner(i, iterator, Seq.empty)
  }

}
