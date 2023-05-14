package hackerrank.tasks.fp.functionalstructures.swapnodes.grisha

import scala.io.StdIn

object SwapNodes2 {
  sealed trait Tree
  final case class Node(value: Int, left: Tree = EmptyNode, right: Tree = EmptyNode) extends Tree
  final object EmptyNode extends Tree

  def buildTree(nodes: Seq[(Int, Int)]): Tree = ???

  def getInorderTraversal(tree: Tree): String = tree match {
    case Node(value, left, right) => getInorderTraversal(left) + s"$value " +
        getInorderTraversal(right)
    case EmptyNode => ""
  }

  def swapNodes(tree: Tree, depth: Int): Tree = {
    def inner(tree: Tree, currentDepth: Int): Tree = tree match {
      case Node(value, left, right) if currentDepth == depth => Node(value, right, left)
      case Node(value, left, right) =>
        Node(value, inner(left, currentDepth + 1), inner(right, currentDepth + 1))
      case EmptyNode => EmptyNode
    }

    inner(tree, 1)
  }

  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt()
    val nodes = (1 to n).map(_ => StdIn.readLine().split("\\w+").toList).collect {
      case h1 :: h2 :: _ => (h1.toInt, h2.toInt)
    }
    val t = StdIn.readInt()
    val swaps = (1 to t).map(_ => StdIn.readInt())

    val tree: Tree = Node(1, Node(2, EmptyNode, Node(4)), Node(3, EmptyNode, Node(5)))
    val newTrees = swaps.scanLeft(tree)((newTree, depth) => swapNodes(newTree, depth))
      .map(getInorderTraversal)
    println(newTrees.mkString(System.lineSeparator()))
  }

}
