package assessments.youtube

import scala.annotation.tailrec

object FindTheLargestPath extends App {
  trait Monoid[T] {
    def empty: T
    def combine(a: T, b: T): T
  }

  object Monoid {
    def apply[T: Monoid]: Monoid[T] = implicitly[Monoid[T]]
  }

  implicit class MonoidOps[T: Monoid](val a: T) {
    def |+|(value: T): T = Monoid[T].combine(a, value)
  }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(a: Int, b: Int): Int = a + b
  }

  // or use cats.implicits._

  def calculate[T: Monoid: Ordering](tree: TreeNode[T]): T = {
    def loop(leftTree: TreeNode[T]): T = leftTree match {
      case TreeNode.Empty => Monoid[T].empty
      case TreeNode.Leaf(value, left, right) => Ordering[T].max(loop(left), loop(right)) |+| value
    }

    loop(tree)
  }

  def calculateTail[T: Monoid: Ordering](tree: TreeNode[T]): T = {
    @tailrec
    def inner(toVisit: List[(TreeNode[T], T)], maxSum: T): T =
      toVisit match {
        case Nil => maxSum
        case (TreeNode.Empty, sum) :: nextToVisit => inner(nextToVisit, Ordering[T].max(maxSum, sum))
        case (TreeNode.Leaf(value, left, right), sum) :: nextToVisit => inner((left, sum |+| value) :: (right, sum |+| value) :: nextToVisit, maxSum)
      }

    inner(List((tree, Monoid[T].empty)), Monoid[T].empty)
  }


  println(calculate(TreeExamples.exampleTree))
  println(calculateTail(TreeExamples.exampleTree))
}
