package hackerrank.tasks.fp

trait Iterator[+A] {
  def next(): (A, Iterator[A])
  def hasNext: Boolean
  def peek(): A
}

object Iterator {

  def fib: Iterator[Long] = {
    def inner(prev: Long, curr: Long): Iterator[Long] = new Iterator[Long] {
      override def next(): (Long, Iterator[Long]) = (curr, inner(curr, prev + curr))
      override def hasNext: Boolean = true
      override def peek(): Long = curr
    }

    inner(0, 1)
  }

  def odd: Iterator[Int] = {
    def inner(i: Int): Iterator[Int] = new Iterator[Int] {
      override def next(): (Int, Iterator[Int]) = (i, inner(i + 2))
      override def hasNext: Boolean = true
      override def peek(): Int = i
    }

    inner(1)
  }

  def sort[A: Ordering](seq: Seq[A]): Iterator[A] = new Iterator[A] {

    override def next(): (A, Iterator[A]) = {
      val min = seq.min
      (min, sort(seq.diff(Seq(min))))
    }

    override def hasNext: Boolean = seq.nonEmpty
    override def peek(): A = seq.min
  }

  def from[A](seq: A*): Iterator[A] = new Iterator[A] {

    override def next(): (A, Iterator[A]) = seq match {
      case Seq() => throw new UnsupportedOperationException("Next on empty iterator!")
      case Seq(head, tail @ _*) => (head, from(tail: _*))
    }

    override def hasNext: Boolean = seq.nonEmpty

    override def peek(): A = seq match {
      case Seq() => throw new UnsupportedOperationException("Next on empty iterator!")
      case Seq(head, _ @_*) => head
    }

  }

}
