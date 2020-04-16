package hackerrank.tasks.fp.functionalstructures.substrsear.ostap

object Solution {

  def main(args: Array[String]) {
    val testNum = scala.io.StdIn.readLine().toInt
    (1 to testNum)
      .map(_ => runTest(scala.io.StdIn.readLine(), scala.io.StdIn.readLine()))
      .foreach {
        case true  => println("YES")
        case false => println("NO")
      }
  }

  private def runTest(search: String, pat: String): Boolean = {
    val searchSize = search.length
    val patSize = pat.length
    if (searchSize < patSize) { false } else {
      runKMPCalc(search, pat.toVector, searchSize, patSize)
    }
  }

  private def runKMPCalc(search: String,
                         pattern: Vector[Char],
                         searchSize: Int,
                         patSize: Int): Boolean = {
    substrSearchKMP(
      search.toList,
      pattern,
      findPrefixTable(pattern, patSize),
      searchSize,
      patSize
    ) >= 0
  }

  private def findPrefixTable(pattern: Vector[Char],
                              patSize: Int): Vector[Int] = {
    @scala.annotation.tailrec
    def prefixFuncLoop(i: Int, caret: Int, image: Vector[Int]): Vector[Int] = {
      if (i == patSize) { image.prepended(0) } else if (pattern(i) == pattern(
                                                          caret
                                                        )) {
        prefixFuncLoop(i + 1, caret + 1, image.appended(caret + 1))
      } else if (caret == 0) {
        prefixFuncLoop(i + 1, caret, image.appended(0))
      } else {
        prefixFuncLoop(i + 1, image(caret - 1), image)
      }
    }
    prefixFuncLoop(1, 0, Vector[Int]())
  }

  private def substrSearchKMP(search: List[Char],
                              pattern: Vector[Char],
                              image: Vector[Int],
                              searchSize: Int,
                              patSize: Int): Int = {
    val lastPatSymbolPos = patSize - 1
    @scala.annotation.tailrec
    def searchKMPloop(searchList: List[Char], caret: Int): Int = {
      if (searchList.isEmpty) {
        -1
      } else if (searchList.head != pattern(caret)) {
        caret match {
          case 0 => searchKMPloop(searchList.tail, caret)
          case _ => searchKMPloop(searchList, image(caret - 1))
        }
      } else {
        if (caret == lastPatSymbolPos) {
          +1
        } else {
          searchKMPloop(searchList.tail, caret + 1)
        }
      }
    }
    searchKMPloop(search, 0)
  }
}
