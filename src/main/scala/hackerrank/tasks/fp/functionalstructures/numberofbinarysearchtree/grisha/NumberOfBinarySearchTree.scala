//package hackerrank.tasks.fp.functionalstructures.numberofbinarysearchtree.grisha
//
//import scala.io.StdIn
//
//object NumberOfBinarySearchTree {
//
//  def catalan(n: Int, fact: LazyList[BigInt]): BigInt = {
//    def binomialCoeff(n: Int, k: Int): BigInt = {
//      val newK = k.min(n - k)
//      fact(n) / (fact(newK) * fact(n - newK))
//    }
//
//    binomialCoeff(2 * n, n) / (n + 1)
//  }
//
//  def main(args: Array[String]): Unit = {
//    val fact: LazyList[BigInt] = {
//      def inner(i: BigInt, prev: BigInt): LazyList[BigInt] = prev #:: inner(i + 1, prev * i)
//
//      inner(1, 1)
//    }
//    val modulo = 100_000_007
//
//    val inputs = (1 to StdIn.readInt()).map(_ => StdIn.readInt)
//    val results = inputs.map(i => catalan(i, fact) % modulo)
//    val output = results.mkString(System.lineSeparator())
//    println(output)
//  }
//
//  private def evalBSTElement(i: Int): BigInt = {
//    if (i > 1) (0 until i).map(k => bst(k) * bst(n - k - 1)).sum
//    else 1
//  }
//
//}
