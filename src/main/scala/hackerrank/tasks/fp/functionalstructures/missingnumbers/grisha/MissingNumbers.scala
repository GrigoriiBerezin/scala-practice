package hackerrank.tasks.fp.functionalstructures.missingnumbers.grisha

import scala.io.StdIn

object MissingNumbers extends App {
  def readList(): List[Int] = {
    val _ = StdIn.readInt()
    StdIn.readLine().trim.split(" ").map(_.toInt).toList
  }

  def findMissingNumbers(refList: List[Int], compList: List[Int]): List[Int] = {
    val refMap: Map[Int, Int] = refList.groupMapReduce(identity)(identity)(_ + _)
    val compMap: Map[Int, Int] = compList.groupMapReduce(identity)(identity)(_ + _)
    compMap.foldLeft[List[Int]](Nil) { case (acc, (key, value)) =>
      val valueCount = refMap.getOrElse(key, 0)
      if ((value - valueCount) > 0) key :: acc else acc
    }.sorted
  }

  val referenceList = readList()
  val compareList = readList()

  val result = findMissingNumbers(referenceList, compareList)
  println(result.mkString(" "))
}
