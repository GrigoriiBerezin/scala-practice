package hackerrank.tasks.fp.functionalstructures.dllist.grisha

sealed trait DLList[+A] {
  def value: A
  def prev: DLList[A]
  def next: DLList[A]
}

object DLEmpty extends DLList[Nothing] {
  override def value: Nothing = throw new NoSuchMethodError()
  override def prev: DLList[Nothing] = throw new NoSuchMethodError()
  override def next: DLList[Nothing] = throw new NoSuchMethodError()
}

final class DLCons[+A](override val value: A, p: => DLList[A], n: => DLList[A]) extends DLList[A] {
  override lazy val prev: DLList[A] = p
  override lazy val next: DLList[A] = n
}
