package sorting

trait SortAlgorithm {
  def sort[T: Ordering](list: List[T]): List[T]
}
