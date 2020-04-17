package hackerrank.tasks.fp.functionalstructures.swapnodes.nikitam

import scala.io.StdIn
import scala.reflect.ClassTag

sealed trait Tree[+T]
final case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
case object Leaf extends Tree[Nothing]

final case class Child[T](l: T, r: T)

object Solution {

  def main(args: Array[String]): Unit = {

    val n = StdIn.readInt()
    val childrenStr = Vector.fill(n)(StdIn.readLine())
    val t = StdIn.readInt()
    val ks = Vector.fill(t)(StdIn.readInt())

    val children = childrenStr.map(parseChild(_, _.toInt))
    val rootValue = 1
    val treeParser = new TreeParser[Int](_ == -1)
    val treeOpt: Option[Tree[Int]] = treeParser.parse(rootValue, children)

    val initialTree: Tree[Int] = treeOpt.getOrElse(throw new IllegalArgumentException("Oh no! Incorrect input"))

    val swappedTrees =
      ks.scanLeft(initialTree)(swap)
        .tail
        .map(toSeq)
        .map(_.mkString(" "))
        .mkString(System.lineSeparator)

    println(swappedTrees)

  }

  private def parseChild[T: ClassTag](string: String, parser: String => T): Child[T] = {
    val Array(l, r) = string.split(" ").map(parser)
    Child(l, r)
  }

  private def swap[T](tree: Tree[T], k: Int): Tree[T] = {

    def go(tree: Tree[T], acc: Int): Tree[T] = tree match {
      case Leaf => Leaf
      case Node(value, left, right) =>
        val updatedLeft = go(left, acc + 1)
        val updatedRight = go(right, acc + 1)

        val (swappedLeft, swappedRight) =
          if (acc % k == 0) (updatedRight, updatedLeft)
          else (updatedLeft, updatedRight)

        Node(value, swappedLeft, swappedRight)
    }

    go(tree, 1)
  }

  private def toSeq[T](tree: Tree[T]): Seq[T] = tree match {
    case Node(value, left, right) => toSeq(left) ++ Vector(value) ++ toSeq(right)
    case Leaf => Vector.empty
  }

}

class TreeParser[T](isLeafValue: T => Boolean) {

  private sealed trait TreeBuilder
  private final case class NodeBuilder(value: T, children: Option[ChildBuilder]) extends TreeBuilder
  private final case object LeafBuilder extends TreeBuilder

  private final case class ChildBuilder(left: TreeBuilder, right: TreeBuilder)

  def parse(rootValue: T, children: Seq[Child[T]]): Option[Tree[T]] = {
    val builderOpt = toTreeBuilder(rootValue, children)
    builderOpt.flatMap(build)
  }

  private def build(builder: TreeBuilder): Option[Tree[T]] = builder match {
    case LeafBuilder => Some(Leaf)
    case NodeBuilder(value, childrenOpt) =>
      for {
        ChildBuilder(leftBuilder, rightBuilder) <- childrenOpt
        left <- build(leftBuilder)
        right <- build(rightBuilder)
      } yield Node(value, left, right)
  }

  private def toTreeBuilder(rootValue: T, children: Seq[Child[T]]): Option[TreeBuilder] = {
    val initialTree: TreeBuilder = NodeBuilder(rootValue, None)
    val initialHeight: Int = 1

    children.foldLeft(Option(initialTree, initialHeight)) { (treeWithHeight, child) =>
      treeWithHeight.flatMap { case (tree, height) =>
        insertChild(tree, height, child).orElse(insertChild(tree, height + 1, child))
      }
    }.map { case (builder, _) => builder }
  }

  private def insertChild(tree: TreeBuilder, maxHeight: Int, child: Child[T]): Option[(TreeBuilder, Int)] =
    insertChildAtHeight(tree, 1, maxHeight, child).map((_, maxHeight))

  private def insertChildAtHeight(treeBuilder: TreeBuilder, currHeight: Int, maxHeight: Int, child: Child[T]): Option[TreeBuilder] =
    treeBuilder match {
      case NodeBuilder(value, None) if currHeight == maxHeight =>

        val leftBuilder = toBuilder(child.l)
        val rightBuilder = toBuilder(child.r)
        Some(NodeBuilder(value, Some(ChildBuilder(leftBuilder, rightBuilder))))

      case NodeBuilder(value, Some(ChildBuilder(left, right))) if currHeight < maxHeight =>

        def insert(tree: TreeBuilder, insertToParent: TreeBuilder => TreeBuilder): Option[TreeBuilder] =
          insertChildAtHeight(tree, currHeight + 1, maxHeight, child).map(insertToParent)
        lazy val insertLeft: Option[TreeBuilder] =
          insert(left, updatedLeft => NodeBuilder(value, Some(ChildBuilder(updatedLeft, right))))
        lazy val insertRight: Option[TreeBuilder] =
          insert(right, updatedRight => NodeBuilder(value, Some(ChildBuilder(left, updatedRight))))

        insertLeft.orElse(insertRight)

      case _ => None
    }

  private def toBuilder(value: T): TreeBuilder =
    if (isLeafValue(value)) LeafBuilder else NodeBuilder(value, None)

}
