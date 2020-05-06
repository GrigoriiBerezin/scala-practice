package hackerrank.tasks.fp.functionalstructures.prisontransport.ostap

import scala.collection.immutable.TreeSet

final case class Chain(inmatesSet: Set[Int])

final case class Chained(groups: Set[Chain], inmates: Set[Int])

object Solution {

  val delimiter = "\\s+"

  def main(args: Array[String]) {
    val inmatesNum = scala.io.StdIn.readLine().toInt
    val pairsNum = scala.io.StdIn.readLine().toInt
    val pairsArr: IndexedSeq[(Int, Int)] = (1 to pairsNum).map(
      _ =>
        scala.io.StdIn
          .readLine()
          .split(delimiter) match {
          case Array(s1, s2) =>
            (s1.toInt, s2.toInt) //перевожу в пару, так как в коде понятнее что работаю еще с исходными парами из инпута
        }
    )
    val pairsList = pairsArr.toList
    //    val startPoint = findStartPoint(pointsArr.toList)
    //    val z = findPath(pointsList, startPoint._1, startPoint._2)

    val chains = makeChains(pairsList)
    val res = getCost(inmatesNum, chains._1, chains._2)
    println(res)

  }

  def makeChains(pairsList: List[(Int, Int)]): (Set[Chain], Int) = {

    @scala.annotation.tailrec
    def makeChainLoop(pList: List[(Int, Int)],
                      aggr: Set[Chain] = Set(),
                      chainIds: Set[Int] = Set()): (Set[Chain], Int) = {
      //      println("---")
      pList match {
        case Nil => (aggr, chainIds.size)
        case head :: tail =>
          val chained: Chained = chain(head, aggr, chainIds)
          makeChainLoop(tail, chained.groups, chained.inmates)
      }
    }

    def chain(pair: (Int, Int),
              chainsSet: Set[Chain],
              chainIds: Set[Int]): Chained = {
      val matchChain = new PartialFunction[Chain, Set[Chain]] {
        def apply(ch: Chain): Set[Chain] = {
          chainsSet - ch + Chain(ch.inmatesSet + pair._1 + pair._2)
        }
        def isDefinedAt(ch: Chain): Boolean = {
          (Set(pair._1, pair._2) intersect ch.inmatesSet).size == 1
        }
      }
      val maybeChainSet: Option[Set[Chain]] = chainsSet.collectFirst(matchChain)
      val res = maybeChainSet match {
        case Some(updChSet) =>
          Chained(updChSet, chainIds ++ Set(pair._1, pair._2))
        case None =>
          Chained(
            chainsSet + Chain(Set(pair._1, pair._2)),
            chainIds ++ Set(pair._1, pair._2)
          )
      }
      res
    }
    makeChainLoop(pairsList)
  }

  def getCost(inmatesNum: Int, groups: Set[Chain], chained: Int): Int = {

    def sqrt(n: Int): Int = (0 to n).dropWhile(x => x * x < n).head

    def getBusCost(chain: Chain): Int = {
      //      println("busCost=" + sqrt(chain.inmatesSet.size))
      sqrt(chain.inmatesSet.size)
    }
    //    println(groups)
    //    println(
    //      "groups.map(ch => getBusCost(ch)).sum=" + groups.toList
    //        .map(ch => getBusCost(ch))
    ////        .foldLeft(0)(_ + _)
    //        .sum
    //    )
    //    println("inmatesNum - chained=" + (inmatesNum - chained))

    groups.toList.map(ch => getBusCost(ch)).sum + inmatesNum - chained
  }

  //  chain(head, aggr)= HashSet(Chain(Set(12, 1)), Chain(Set(5, 3)), Chain(Set(1, 2, 4)), Chain(Set(1, 2, 12)), Chain(Set(1, 2, 4, 12)), Chain(Set(1, 2)))

}