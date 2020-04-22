package hackerrank.tasks.fp.functionalstructures.validbst.grisha

import scala.io.StdIn.{readInt, readLine}

object ValidBST {
  def isValidBST(head: Int,
                 path: List[Int],
                 buffer: List[Int] = Nil,
                 min: Int = Int.MinValue,
                 max: Int = Int.MaxValue): Boolean =
    path match {
      case node :: nodes =>
        if (node < min || node > max) false
        else if (node < head) isValidBST(head, nodes, buffer :+ node)
        else if (buffer.isEmpty) isValidBST(node, nodes, min = head, max = max)
        else
          isValidBST(buffer.head, buffer.tail, min = min, max = head) &&
          isValidBST(node, nodes, min = head, max = max)
      case Nil =>
        if (buffer.isEmpty) true
        else isValidBST(buffer.head, buffer.tail, min = min, max = max)
    }

  def main(args: Array[String]): Unit =
    (for (_ <- readInt() until 0 by -1) yield {
      readInt()
      val nodes = readLine().trim.split(" ").map(n => n.toInt).toList
      isValidBST(nodes.head, nodes.tail)
    }).foreach(x => if (x) println("YES") else println("NO"))
}
