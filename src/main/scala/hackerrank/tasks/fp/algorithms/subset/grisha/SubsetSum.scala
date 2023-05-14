package hackerrank.tasks.fp.algorithms.subset.grisha

import scala.annotation.tailrec
import scala.collection.Searching
import scala.io.StdIn

object SubsetSum {

  private def subsetSum(lowBorder: Int, selection: Seq[Int]): Int = {
    @tailrec def inner(restSelection: List[Int], acc: Seq[Int]): Int = restSelection match {
      case head :: _ if acc.sum + head >= lowBorder => acc.length + 1
      case head :: tail => inner(tail, acc :+ head)
      case Nil => -1
    }

    inner(selection.sorted(Ordering[Int].reverse).toList, Seq.empty)
  }

  private def subsetSum2(lowBorder: Int, selection: Array[Int]): Int = {
    @tailrec def inner(restSelection: List[Int], acc: Int, count: Int): Int = restSelection match {
      case head :: _ if acc + head >= lowBorder => count + 1
      case head :: tail => inner(tail, acc + head, count + 1)
      case Nil => -1
    }

    inner(selection.sorted(Ordering[Int].reverse).toList, 0, 0)
  }

  private def subsetSum3(lowBorder: Int, selection: Seq[Int]): Int = {
    @tailrec def inner(restSelection: Seq[Int], acc: Int, count: Int): Int =
      if (restSelection.isEmpty) -1
      else restSelection.find(value => acc + value >= lowBorder) match {
        case Some(_) => count + 1
        case None =>
          val max = restSelection.max
          inner(restSelection.diff(Seq(max)), acc + max, count + 1)
      }

    inner(selection, 0, 0)
  }

  private def subsetSum4(lowBorder: Int, selection: Seq[Int]): Int = {
    def inner(restSelection: List[Int], acc: Int, count: Int, subsets: List[Int]): List[Int] =
      restSelection match {
        case head :: _ if acc + head >= lowBorder => (count + 1) +: subsets
        case head :: tail => inner(tail, acc + head, count + 1, List.empty) ++
            inner(tail, acc, count, List.empty)
        case Nil => List.empty
      }

    val subsets = inner(selection.toList, 0, 0, List.empty)
    if (subsets.isEmpty) -1 else subsets.min
  }

  private def subsetSum5(lowBorder: Long, selection: IndexedSeq[Long]): Int = {
    selection.search(lowBorder) match {
      case Searching.Found(foundIndex) => foundIndex
      case Searching.InsertionPoint(insertionPoint) =>
        if (insertionPoint >= selection.length) -1 else insertionPoint
    }
  }

  def main(args: Array[String]): Unit = {
    val _ = StdIn.readInt()
    val a = StdIn.readLine().split(" ").map(_.toInt)
    val t = StdIn.readInt()
    val s = (1 to t).map(_ => StdIn.readLong())
    val cashedSelection = a.sorted(Ordering[Int].reverse).scanLeft(0L)(_ + _)

    val result = s.map(subsetSum5(_, cashedSelection))
    val validResult = result.mkString(System.lineSeparator())
    println(validResult)
  }

}
