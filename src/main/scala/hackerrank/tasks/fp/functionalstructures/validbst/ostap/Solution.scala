package hackerrank.tasks.fp.functionalstructures.validbst.ostap

object Solution {

  import scala.collection.immutable.List

  private val delimiter = " "

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val testNum = scala.io.StdIn.readLine().toInt
    val result = (1 to testNum)
      .map { _ =>
        scala.io.StdIn.readLine()
        testIfValidBst(readListFromString(scala.io.StdIn.readLine()))
      } //andthen

    result.foreach {
      case true => println("YES")
      case false => println("NO")
    }
  }

  private def readListFromString(numsStr: String): List[Int] = {
    numsStr.split(delimiter).map(_.trim.toInt).toList
  }

  private def testIfValidBst(nums: List[Int]): Boolean = {

    @scala.annotation.tailrec
    def traversal(numsList: List[Int], branchAggr: List[Int]): Boolean = {
      numsList match {
        case x :: y :: _ =>
          if (x < y) {
            traversal(numsList.tail, branchAggr.prepended(x))
          } else {
            branchAggr.nonEmpty && traversal(numsList.tail, branchAggr.tail)
          }
        case _ :: Nil => true
      }
    }

    traversal(nums, List())
  }

}

/*
5
3
1 2 3
3
2 1 3
6
3 2 1 5 4 6
4
1 3 4 2
5
3 4 5 1 2
 */