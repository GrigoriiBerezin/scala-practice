package sorting.insertionsort

import sorting.SortAlgorithm

object InsertionSort extends SortAlgorithm {
  override def sort[T: Ordering](list: List[T]): List[T] = {
    def sortedInsert(element: T, sorted: List[T]): List[T] = {
      val (min, sortedList) = sorted.foldRight((element, List.empty[T])) {
        case (listElement, (min, sortedList)) if Ordering[T].gt(listElement, min) => (min, listElement :: sortedList)
        case (listElement, (min, sortedList)) => (listElement, min :: sortedList)
      }
      min :: sortedList
    }

    @scala.annotation.tailrec
    def loop(sorted: List[T], toSort: List[T]): List[T] = toSort match {
      case h :: t => loop(sortedInsert(h, sorted), t)
      case Nil => sorted
    }

    loop(List.empty[T], list)
  }
}
