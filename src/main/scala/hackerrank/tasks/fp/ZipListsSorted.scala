package hackerrank.tasks.fp

import scala.annotation.tailrec

object ZipListsSorted extends App {

  def sortTwoSortedLists[A: Ordering](l1: List[A], l2: List[A]): List[A] = {
    @tailrec
    def inner(l1: List[A], l2: List[A], acc: List[A]): List[A] = (l1, l2) match {
      case (h1 :: t1, h2 :: t2) =>
        if (Ordering[A].lt(h1, h2)) inner(t1, l2, h1 :: acc) else inner(l1, t2, h2 :: acc)
      case (Nil, h2 :: t2) => inner(Nil, t2, h2 :: acc)
      case (h1 :: t1, Nil) => inner(t1, Nil, h1 :: acc)
      case (Nil, Nil) => acc
    }

    def inner2(l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (h1 :: t1, h2 :: t2) =>
        if (Ordering[A].lt(h1, h2)) h1 :: inner2(t1, l2) else h2 :: inner2(l1, t2)
      case (Nil, _ :: _) => l2
      case (_ :: _, Nil) => l1
      case (Nil, Nil) => Nil
    }

    def inner3(l1: List[A], l2: List[A]): List[A] = (l1 ++ l2).sorted

    inner2(l1, l2)
  }

  private val l1 = List(1, 2, 3, 4)
  private val l2 = List(5, 6, 7, 8)

  println(sortTwoSortedLists(l1, l2))
}
