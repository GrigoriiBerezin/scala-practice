package hackerrank.tasks.fp.functionalstructures.numbinarysearch.nikitam

import scala.io.StdIn.readInt

object Solution {

  def main(args: Array[String]): Unit = {
    val amountOfTrees = readInt
    val treeSizes = List.fill(amountOfTrees)(readInt)

    val answer =
      treeSizes
        .map(treeSizeToCombinationsAmount)
        .map(normalize)
        .mkString(System.lineSeparator)

    println(answer)
  }

  val normalize: BigInt => BigInt = _ % 100000007

  val treeSizeToCombinationsAmount: LazyList[BigInt] = LazyList.from(0).map(getCombinationsAmount)

  def getCombinationsAmount(treeSize: Int): BigInt =
    if (treeSize > 0) {
      (0 until treeSize)
        .map(rootPos => treeSizeToCombinationsAmount(rootPos) * treeSizeToCombinationsAmount(treeSize - (rootPos + 1)))
        .sum
    } else {
      1
    }
}
