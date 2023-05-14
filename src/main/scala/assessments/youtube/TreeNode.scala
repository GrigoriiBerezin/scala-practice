package assessments.youtube

sealed trait TreeNode[+T]

object TreeNode {
  final object Empty extends TreeNode[Nothing]
  final case class Leaf[T](value: T, left: TreeNode[T], right: TreeNode[T]) extends TreeNode[T]
}
