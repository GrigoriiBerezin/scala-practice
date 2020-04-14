package hackerrank.tasks.fp.functionalstructures.swapnodes.ilia

import scala.io.StdIn

object Solution extends App {
  final case class Input(n: Int, nodes: List[String], k: Int, swaps: List[Int])

  sealed trait Tree {
    def add(tree: Tree): Tree
  }

  final case class Node(value: Int, left: Tree, right: Tree) extends Tree {
    override def add(tree: Tree): Tree = tree match {
      case Leaf => this
      case Node(tV, _, _) if tV == value => tree
      case node => Node(value, left.add(node), right.add(node))
    }
  }

  final case object Leaf extends Tree {
    override def add(tree: Tree): Tree = Leaf
  }

  final case object EmptyTree extends Tree {
    override def add(tree: Tree): Tree = tree
  }

  object Tree {
    def fromStr(str: String): Tree = str match {
      case "-1" => Leaf
      case s => Node(s.toInt, Leaf, Leaf)
    }
  }

  def readInput(): Input = {
    val n = StdIn.readLine().toInt
    val nodes = List.fill(n)(StdIn.readLine())
    val k = StdIn.readLine().toInt
    val swaps = List.fill(k)(StdIn.readLine().toInt)

    Input(n, nodes, k, swaps)
  }

  def buildTree(nodes: List[String]): Tree =
    nodes
      .zip(LazyList.from(1))
      .foldLeft[Tree](EmptyTree) { case (tree, (str, index)) =>
        val Array(fst, snd) = str.split(" ")
        tree.add(Node(index, Tree.fromStr(fst), Tree.fromStr(snd)))
      }

  def inOrderTraversal(tree: Tree): List[Int] = tree match {
    case Node(value, left, right) => inOrderTraversal(left) ++ List(value) ++ inOrderTraversal(right)
    case Leaf => Nil
  }

  def maxDepth(tree: Tree): Int = tree match {
    case Leaf => 0
    case Node(_, left, right) => Math.max(maxDepth(left), maxDepth(right)) + 1
  }

  def swapNodes(tree: Tree, k: Int): Tree = {
    def innerSwap(innerTree: Tree, k: Int, level: Int, maxDepth: Int): Tree = innerTree match {
      case Node(value, left, right) if level % k == 0 && level <= maxDepth =>
        Node(value, innerSwap(right, k, level + 1, maxDepth), innerSwap(left, k, level + 1, maxDepth))
      case node if level % k == 0 => node
      case Node(value, left, right) => Node(value, innerSwap(left, k, level + 1, maxDepth), innerSwap(right, k, level + 1, maxDepth))
      case Leaf => Leaf
    }

    innerSwap(tree, k, 1, maxDepth(tree))
  }

  def outputResult(tree: Tree, swaps: List[Int]): List[String] = {
    @scala.annotation.tailrec
    def innerOutput(tree: Tree, swaps: List[Int], outputs: List[String]): List[String] = swaps match {
      case Nil => outputs
      case x :: xs =>
        val swappedTree = swapNodes(tree, x)
        innerOutput(swappedTree, xs, outputs :+ inOrderTraversal(swappedTree).mkString(" "))
    }

    innerOutput(tree, swaps, List.empty[String])
  }

  val input = readInput()

  val tree = buildTree(input.nodes)

  outputResult(tree, input.swaps).foreach(println)
}
