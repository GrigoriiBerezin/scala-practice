package hackerrank.tasks.fp.functionalstructures.superdigit.ostap

object Solution {

  def main(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named HRMagicDigit.Solution*/
    val stdarr = scala.io.StdIn.readLine().split(" ")
    val n = stdarr(0)
    val k = stdarr(1).toInt
    var res: Int = -1
    if (k % 10 == 0) { //*10 do not influence the result   getSuperDigit(1456)=16=7 getSuperDigit(1456)*10=7*10=7 getSuperDigit(1456)*1000=7*1000=7
      res = getSuperDigit(getSumString(n.toList, 0))
    } else {
      res = getSuperDigit(getSumString(n.toList, 0) * k)
    }
    println(res)
  }

  @scala.annotation.tailrec
  def getSumString(s: List[Char], aggr: Int): Int = {
    s match {
      case Nil          => aggr
      case head :: tail => getSumString(tail, head.asDigit + aggr)
    }
  }

  @scala.annotation.tailrec
  def getSuperDigit(digit: Int): Int = {
    if (digit.toString.length <= 1) digit
    else getSuperDigit(digit.toString.map(_.asDigit).sum)
  }
}igit(digit.toString.map(_.asDigit).sum)
  }
}
