package sorting.cocktailshakesort

import sorting.SortAlgorithm

import scala.annotation.tailrec

object CocktailShakerSort extends App with SortAlgorithm {
  override def sort[T: Ordering](list: List[T]): List[T] = {

    def find(source: List[T], predicate: (T, T) => Boolean): (List[T], Option[T]) = source
      .foldLeft((List.empty[T], Option.empty[T])) {
        case ((result, None), listElement) => (result, Some(listElement))
        case ((result, fountOpt @ Some(found)), listElement) =>
          if (predicate(found, listElement)) (listElement :: result, fountOpt)
          else (found :: result, Some(listElement))
      }

    def findRight(source: List[T]): (List[T], Option[T]) = find(source, Ordering[T].gt)

    def findLeft(source: List[T]): (List[T], Option[T]) = find(source, Ordering[T].lt)

    @tailrec def loop(source: List[T], mins: List[T], maxs: List[T]): List[T] = {
      val (rest, maxOpt) = findRight(source)
      maxOpt match {
        case Some(max) =>
          val (rest2, minOpt) = findLeft(rest)
          minOpt match {
            case Some(min) => loop(rest2, mins :+ min, max :: maxs)
            case None => mins ++ (max :: maxs)
          }
        case None => mins ++ maxs
      }
    }

    if (list.length == 1) list
    else loop(list, List.empty[T], List.empty[T])
  }

  // back and forward
  // 6 4 2 1 7 8
  // 4 2 1 6 7 (max 8)
  // 4 2 6 7 (min 1), (max 8)
  // 2 4 6 (min 1) (max 7, 8)

  // only forward
  // 6 4 2 1 7 8
  // 4 2 1 6 7 (max 8)
  // 2 1 4 6 (max 7, 8)
  // 1 2 4 (max 6, 7, 8)

}
