package hackerrank.tasks.fp.functionalstructures.superdigit.ostap

object Solution {

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution*/
    val stdarr = scala.io.StdIn.readLine().split(" ")
    val n = stdarr(0).map(_.asDigit).sum
    val k = stdarr(1).toInt
    if (k % 10 == 0) { //*10 do not influence the result   getSuperDigit(1456)=16=7 getSuperDigit(1456)*10=7*10=7 getSuperDigit(1456)*1000=7*1000=7
      println(getSuperDigit(getSuperDigit(n)))
    } else {
      println(getSuperDigit(getSuperDigit(n) * k))
    }
  }

  def getSumString(s: List[Char], aggr: Int): Int = {
    s match {
      case Nil          => aggr
      case head :: tail => getSumString(tail, head.asDigit + aggr)
    }
  }

  def getSuperDigit(digit: Int): Int = {
    if (digit.toString.size <= 1) digit
    else getSuperDigit(digit.toString.map(_.asDigit).sum)
  }

}
