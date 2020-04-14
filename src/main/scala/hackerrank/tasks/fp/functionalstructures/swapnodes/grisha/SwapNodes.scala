package hackerrank.tasks.fp.functionalstructures.swapnodes.grisha

import scala.io.StdIn.{readLine, readInt}

object SwapNodes {
  sealed trait Tree
  final case class Node(value: Int,
                        left: Tree = EmptyNode,
                        right: Tree = EmptyNode)
      extends Tree
  final case object EmptyNode extends Tree

  // for normal input when left full-filled and then right
  def getTree(value: Int): Tree = {
    if (value == -1) EmptyNode
    else
      readLine().trim().split(" ").map(_.toInt).toList match {
        case node1 :: node2 :: _ => Node(value, getTree(node1), getTree(node2))
        case _                   => throw new Exception("Something wrong")
      }
  }

  // for task input full-filled by depth
  def buildTree(nodes: List[String]): Tree = ???

  def inorderTraversal(root: Tree): List[Int] =
    root match {
      case Node(value, left, right) =>
        inorderTraversal(left) ++ (value :: inorderTraversal(right))
      case EmptyNode => Nil
    }

  def swapNodes(root: Tree, depth: Int): Tree = {
    def innerSwapNodes(root: Tree, level: Int): Tree =
      root match {
        case Node(value, left, right) =>
          if (level % depth == 0)
            Node(
              value,
              innerSwapNodes(right, level + 1),
              innerSwapNodes(left, level + 1)
            )
          else
            Node(
              value,
              innerSwapNodes(left, level + 1),
              innerSwapNodes(right, level + 1)
            )
        case EmptyNode => root
      }

    innerSwapNodes(root, 1)
  }

  def getOrders(root: Tree, range: Int): List[String] = {
    def innerGetOrders(root: Tree, count: Int): List[String] =
      if (count > range) Nil
      else {
        val depth = readInt()
        val newRoot = swapNodes(root, depth)
        innerGetOrders(newRoot, count + 1) :+ inorderTraversal(newRoot)
          .mkString("\\s")
      }

    innerGetOrders(root, 1)
  }

  def main(args: Array[String]): Unit = {
    val size = readInt()
    val nodes = List.fill(size)(readLine())
    val range = readInt()
    for (order <- getOrders(buildTree(nodes), range)) println(order)
  }
}
