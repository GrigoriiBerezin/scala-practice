package hackerrank.tasks.fp.zip

sealed trait Tree[A]

object Tree {
  final case class Leaf[A](value: A, weight: Int) extends Tree[A]

  final case class Branch[A](left: Tree[A], right: Tree[A], value: List[A], weight: Int)
      extends Tree[A]

  def value[A](tree: Tree[A]): List[A] = tree match {
    case Leaf(value, _) => List(value)
    case Branch(_, _, value, _) => value
  }

  def weight[A](tree: Tree[A]): Int = tree match {
    case Leaf(_, weight) => weight
    case Branch(_, _, _, weight) => weight
  }

  def merge[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right, value(left) ++ value(right), weight(left) + weight(right))

}
