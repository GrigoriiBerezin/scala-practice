package hackerrank.tasks.fp.functionalstructures.prisontransport.ostap

import scala.collection.immutable.TreeSet

final case class Chain(inmatesSet: TreeSet[Int])

final case class Chained(groups: TreeSet[Chain], inmates: TreeSet[Int])

object Solution {

  implicit val chainOrdering = new Ordering[Chain] {
    def compare(c1: Chain, c2: Chain): Int = {
      c1.inmatesSet.head - c2.inmatesSet.head
    }
  }

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
    val chains = makeChains(pairsList)
    val res = getCost(inmatesNum, chains._1, chains._2)
    println(res)

  }

  def makeChains(pairsList: List[(Int, Int)]): (Set[Chain], Int) = {

    @scala.annotation.tailrec
    def makeChainLoop(
      pList: List[(Int, Int)],
      aggr: TreeSet[Chain] = TreeSet[Chain](),
      chainIds: TreeSet[Int] = TreeSet[Int]()
    ): (Set[Chain], Int) = {
      pList match {
        case Nil => (aggr, chainIds.size)
        case head :: tail =>
          val chained: Chained = chain(head, aggr, chainIds)

          makeChainLoop(tail, chained.groups, chained.inmates)
      }
    }

    def chain(pair: (Int, Int),
              chainsSet: TreeSet[Chain],
              chainIds: TreeSet[Int]): Chained = {
      def hasIntersections(ch: Chain): Boolean = {
        (Set(pair._1, pair._2) intersect ch.inmatesSet).size >= 1 //1 or 2
      }

      val chainsIntersectedPairs = chainsSet.filter(ch => hasIntersections(ch))
      if (chainsIntersectedPairs.size > 1) {
        val updatedChains: TreeSet[Chain] = chainsIntersectedPairs.size match {
          case 1 =>
            chainsSet - chainsIntersectedPairs.head + Chain(
              chainsIntersectedPairs.head.inmatesSet + pair._1 + pair._2
            )
          //        case 2 => TreeSet(chainsIntersectedPairs.foldLeft((cOne: Chain, cTwo: Chain) => Chain(TreeSet(cOne.inmatesSet union cTwo.inmatesSet))))
          case 2 =>
            val ch = chainsIntersectedPairs.reduce(
              (cOne: Chain, cTwo: Chain) =>
                Chain(TreeSet(cOne.inmatesSet union cTwo.inmatesSet))
            )
//            TreeSet(
//              chainsIntersectedPairs.reduceLeft[Chain](
//                (cOne: Chain, cTwo: Chain) =>
//                  Chain(TreeSet(cOne.inmatesSet union cTwo.inmatesSet))
//              )
//            )
          case _ => throw new RuntimeException("too many intersections")
        }
        Chained(updatedChains, chainIds ++ TreeSet(pair._1, pair._2))
      } else {
        Chained(
          chainsSet + Chain(TreeSet(pair._1, pair._2)),
          chainIds ++ TreeSet(pair._1, pair._2)
        )
      }

    }
    makeChainLoop(pairsList)
  }

  def getCost(inmatesNum: Int, groups: Set[Chain], chained: Int): Int = {

    def sqrt(n: Int): Int = (0 to n).dropWhile(x => x * x < n).head

    def getBusCost(chain: Chain): Int = {
      sqrt(chain.inmatesSet.size)
    }
    groups.toList.map(ch => getBusCost(ch)).sum + inmatesNum - chained
  }
}BusCost(ch)).sum + inmatesNum - chained
  }
}
//14
//8
//1 2
//1 4
//5 3
//1 12
//7 6
//8 11
//13 12
//3 14
//************************
//16
//8
//6 11
//9 5
//11 9
//15 9
//13 15
//12 14
//15 16
//1 16
