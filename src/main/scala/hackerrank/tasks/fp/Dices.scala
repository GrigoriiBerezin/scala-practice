package hackerrank.tasks.fp

object Dices extends App {

  case class Dice(curState: Int, downState: Int, rightState: Int) {
    def rollDown: Dice = this.copy(downState, 7 - curState, rightState)

    def rollRight: Dice = this.copy(rightState, downState, 7 - curState)
  }

  object Dice {
    def startState: Dice = Dice(1, 5, 3)
    def zero: Dice = Dice(0, 0, 0)
  }

  lazy val paths: LazyList[LazyList[(Int, Dice)]] = {
    def fillZero(): LazyList[(Int, Dice)] = (0, Dice.zero) #:: fillZero()

    def buildTable(
        x: Int,
        acc: Int,
        dice: Dice,
        prevList: LazyList[(Int, Dice)]
    ): LazyList[LazyList[(Int, Dice)]] = {
      def buildRow(y: Int, leftAcc: Int, leftDice: Dice): LazyList[(Int, Dice)] = {
        val (aboveAcc, aboveDice) = prevList(y)
        val aboveResult = aboveAcc + aboveDice.rollDown.curState
        val leftResult = leftAcc + leftDice.rollRight.curState
        if (aboveResult > leftResult) (aboveResult, aboveDice.rollDown) #::
          buildRow(y + 1, aboveResult, aboveDice.rollDown)
        else (leftResult, leftDice.rollRight) #:: buildRow(y + 1, leftResult, leftDice.rollRight)
      }

      ((0, Dice.zero) #:: (acc, dice) #:: buildRow(1, acc, dice)) #::
        buildTable(x + 1, acc + dice.rollDown.curState, dice.rollDown, paths(x + 1))
    }

    val dice = Dice.startState
    fillZero() #:: buildTable(0, dice.curState, dice, paths.head)
  }

//  val i = StdIn.readInt()
//  val input = (1 to i).map(_ => StdIn.readLine().split("\\s+").map(_.toInt).toList)
//  val result = input.map { case m :: n :: Nil => paths(m)(n) }
//  val output = result.mkString(System.lineSeparator())
//  println(output)

  println(paths.take(5).map(l => l.take(5).toList).toList.map(l => l.map(_._1).mkString(" ")).mkString(System.lineSeparator()))

//  private val data = mutable.Map[(Int, Int, Dice), Int]()
//
//  def main(args: Array[String]): Unit = {
//    val strongestPaths = (1 to readInt).map(_ => readLine().split("\\s").map(_.toInt).toList).map {
//      case m :: n :: Nil =>
//        makeStrongestPath(m, n)
//    }
//
//    strongestPaths.foreach(println)
//  }
//
//  def makeStrongestPath(m: Int, n: Int): Int = {
//    def inner(m: Int, n: Int, dice: Dice, acc: Int): Int = data.getOrElseUpdate((m, n, dice),
//      if (m < 0 || n < 0) 0 else
//        math.max(inner(m - 1, n, dice.rollDown, dice.curState), inner(m, n - 1, dice.rollRight, dice.curState))
//    ) + acc
//
//    inner(m - 1, n - 1, Dice.startState, 0)
//  }
}
