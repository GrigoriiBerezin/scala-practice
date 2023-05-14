package sorting.selectionsort

import sorting.SortAlgorithm

import scala.annotation.tailrec

object SelectionSort extends SortAlgorithm {
  override def sort[T: Ordering](list: List[T]): List[T] = {
    @tailrec
    def loop(max: T, left: List[T], right: List[T], acc: List[T]): List[T] = right match {
      case h :: t if Ordering[T].gt(h, max) => loop(h, max :: left, t, acc)
      case h :: t => loop(max, h :: left, t, acc)
      case Nil => left match {
        case h :: t => loop(h, List.empty[T], t, max :: acc)
        case Nil => max :: acc
      }
    }

    list match {
      case h :: t => loop(h, List.empty[T], t, List.empty[T])
      case Nil => Nil
    }
  }
}
