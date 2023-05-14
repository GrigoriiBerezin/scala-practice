package assessments.youtube

import cats.Monoid
import cats.instances.int._
import cats.syntax.monoid._

object FindTheLargestPathHard extends App {

  def calculate[T: Ordering: Monoid](tree: TreeNode[T]): T = {
    type MaxPathWithBiggest = (T, T)
    def helper(leftTree: TreeNode[T]): MaxPathWithBiggest = leftTree match {
      case TreeNode.Empty => (Monoid[T].empty, Monoid[T].empty)
      case TreeNode.Leaf(value, left, right) =>
        val (maxLeftPath, maxLeftSingle) = helper(left)
        val (maxRightPath, maxRightSingle) = helper(right)
        val maxPath = Ordering[T].max(maxLeftSingle, Monoid[T].empty) |+| Ordering[T].max(maxRightSingle, Monoid[T].empty) |+| value
        val maxSingle = Ordering[T].max(maxLeftSingle, maxRightSingle) |+| value
        (Ordering[T].max(maxPath, Ordering[T].max(maxLeftPath, maxRightPath)), Ordering[T].max(maxSingle, Monoid[T].empty))
    }

    val (maxPath, _) = helper(tree)
    maxPath
  }

  println(calculate(TreeExamples.exampleTree))
}
