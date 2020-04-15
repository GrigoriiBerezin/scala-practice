package hackerrank.tasks.fp.functionalstructures.swapnodes.ostap

/**
A binary tree is a tree which is characterized by any one of the following properties:

    It can be an empty (null).
    It contains a root node and two subtrees, left subtree and right subtree. These subtrees are also binary tree.

Inorder traversal is performed as

    Traverse the left subtree.
    Visit root (print it).
    Traverse the right subtree.

We define depth of a node as follow:

    Root node is at depth 1.
    If the depth of parent node is d, then the depth of current node wll be d+1.
 */

sealed abstract class BinaryTree()

final case class EmptyTree() extends BinaryTree(){
  override def toString() ={
    " [Empty] "
  }
}

final case class NonEmptyTree(num: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree(){
  override def toString() ={
    val main = s"<$num>"
    val leftS= left.toString()
    val rightS = right.toString()
    s" [$main left($leftS) right($rightS)] "
  }

  def setLeftTree(tree: BinaryTree) = {//все еще иммутабельно
    NonEmptyTree(num, tree, right)
  }

  def setRightTree(tree: BinaryTree) ={
    NonEmptyTree(num, left, tree)
  }
}

object Solution {

  val delimiter = "\\s"

  def main(args: Array[String]) {
    val nodesNum = scala.io.StdIn.readLine().toInt
    val nodesArr: List[Array[Int]] = (1 to nodesNum).map(
      _ =>
        (
          scala.io.StdIn
            .readLine()
            .split(delimiter)
            .map(_.trim.toInt)
          )
    ).toList

    val nodeTuplesList = nodesArr.map { case Array(t1, t2) => (t1, t2) }

    val swapsNum = scala.io.StdIn.readLine().toInt
    val swapsArr = (1 to swapsNum).map(_ => scala.io.StdIn.readLine().toInt)

    println("nodesNum=" + nodesNum + " pointsArr=" + nodesArr + " swapsNum=" + swapsNum + " swapsArr=" + swapsArr)
    val slicedToLayers = constructLayersFromNodes(nodeTuplesList)
    println(slicedToLayers)
    println("constructTreeFromNodes(slicedToLayers)="+constructTreeFromNodes(slicedToLayers))
  }

  def constructLayersFromNodes(list: List[(Int, Int)]): List[List[(Int, Int)]] = {
    @scala.annotation.tailrec
    def loopNodes(list: List[(Int, Int)], nodesOnLvl: Int, acc: List[List[(Int, Int)]]): List[List[(Int, Int)]] = { //получим список с разбивкой по уровням
      list match {
        case Nil => acc
        case _ =>
          val (toParse, toParseThen): (List[(Int, Int)], List[(Int, Int)]) = list.splitAt(nodesOnLvl) //берем из списка те ноды что на текущем уровне
          println("nodesOnLvl"+nodesOnLvl)
          println(toParse)
          println(toParseThen)
          val nodesOnNextLvl = toParse.map { case (l, r) => List(l, r).count { case x: Int => x != -1 } }.sum //
          loopNodes(toParseThen, nodesOnNextLvl, acc.prepended(toParse)) //prepended переворачиваем слои снизу вверх
      }
    }
    loopNodes(list, 1, List.empty[List[(Int, Int)]])
  }


  def constructTreeFromNodes(list: List[List[(Int, Int)]]): BinaryTree = { //дерево будем строить наоборот, снизу собирая из кусочков!

    @scala.annotation.tailrec
    def constructLayer(unbounded: List[BinaryTree], toBound : List[(Int, Int)], acc: List[BinaryTree] = List.empty[BinaryTree]): List[BinaryTree] = {
      toBound match {
        case Nil => acc
        case (l,r) :: tail =>
          //            val (l,r): (Int, Int) = tuple
          println("------------tuple--------------")
          println("tuple"+l +"+"+ r)
          println("ACCUMULATOR="+acc)
          println("unbounded="+unbounded+" unbounded.size"+unbounded.size)
          if (unbounded.isEmpty) {println("the end"+NonEmptyTree(1, unbounded(0), unbounded(1)))}
          if((l == -1)&&(r == -1)) {
            constructLayer(unbounded, tail, acc.appended(EmptyTree()))//из заготовок ничего не достаем - с удовольствием зарефакторил бы в функ стиль но пока в голове не удержать все, так пока проще
          } else if ((l == -1)&&(r != -1)) {
            constructLayer(unbounded.drop(2), tail, acc.appendedAll(List(EmptyTree(), NonEmptyTree(r, unbounded(0), unbounded(1)))))}
          else if((r == -1)&&(l != -1)) {
            constructLayer(unbounded.drop(2), tail, acc.appendedAll(List(NonEmptyTree(l, unbounded(0), unbounded(1)), EmptyTree())))}
          else {
            println("two unbounded="+unbounded.drop(4))
            constructLayer(unbounded.drop(4), tail, acc.appendedAll(List(NonEmptyTree(l, unbounded.head, unbounded(1)), NonEmptyTree(r, unbounded(2), unbounded(3)))))//опасный момент с листом не не придумал как иначе делать иммутабельно take
          }
      }
    }

    @scala.annotation.tailrec
    def constructTreeLoop(list: List[List[(Int, Int)]], acc: List[BinaryTree]): List[BinaryTree] = {//получим лист из одного дерева
      list match {
        case Nil => acc //в конце должен получится лист из одного элемента
        case head :: tail =>
          println("============constructTreeLoop==============")
          val newAcc: List[BinaryTree] = constructLayer(acc, head, List.empty[BinaryTree])
          constructTreeLoop(tail, newAcc)
      }
    }

    val initEmptyiesList: List[BinaryTree] = list.head.flatMap { case (l, r) => List(EmptyTree(), EmptyTree()) }
    constructTreeLoop(list.tail, initEmptyiesList)(0)
  }

  //      constructTreeLoop(list, NonEmptyTree(1, EmptyTree(-1), EmptyTree(-1)), 0)


}



//3
//2 3
//-1 -1
//-1 -1
//2
//1
//1
