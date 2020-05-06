package hackerrank.tasks.fp.functionalstructures.numbinarysearch.ostap

object Solution {

  val delimiter = "\\s+"

  def main(args: Array[String]) {
    val testsNum = scala.io.StdIn.readLine().toInt
    val numsList: List[Int] =
      (1 to testsNum).map(_ => scala.io.StdIn.readLine().toInt).toList
    numsList.foreach(n => println(constructLeftTree(n)))

  }

  abstract class BinTree{
    def toString(): String
  }
  case class NonEmptyTree(left: BinTree, right: BinTree) extends BinTree {
    def isEmpty = false
    override def toString(): String = {
      "NE" + "(" + left.toString + ", " + right.toString + ")"
    }
  }
  object EmptyTree extends BinTree {
    override def toString(): String = {
      "E"
    }
    def isEmpty = true
  }

  def constructLeftTree(numNodes: Int): BinTree = {
    def leftTreeGen(n: Int, tree: NonEmptyTree): BinTree = {
      println("tre is=" + tree + " n=" + n)
      if (n == 0) { tree } else {
        leftTreeGen(n - 1, NonEmptyTree(tree, EmptyTree))
      }
    }
    leftTreeGen(numNodes, NonEmptyTree(EmptyTree, EmptyTree))
  }

  def rightiseTree(num: Int, acc: BinTree) = {

    // по слоям вниз
//    def iter(curNode: BinTree, acc: BinTree): BinTree = {
//      curNode match {
//        case NonEmptyTree =>
//        case EmptyTree    =>
//      }
//    }

  }
}
