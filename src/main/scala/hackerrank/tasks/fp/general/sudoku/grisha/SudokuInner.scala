package hackerrank.tasks.fp.general.sudoku.grisha

import scala.annotation.tailrec
import scala.io.StdIn

// Didn't want to change the whole structure of my case class
case class SudokuInner(table: Seq[Seq[Int]], isDiagonal: Boolean = false, sensitive: Double = 40.5) {
  import SudokuInner._

  // Table structure:
  // /--------------> x
  // |100|007|090|
  // |030|020|008|
  // |009|600|500|
  // |-----------|
  // |005|300|900|
  // |010|080|002|
  // |600|004|000|
  // |-----------|
  // |300|000|010|
  // |040|000|007|
  // |007|000|300|
  // v
  // y

  private def checkDiagonal(pos: Position): Seq[Int] =
    if (pos.isLeftDiagonal) {
      (for {
        x <- 0 until tableSize
        y <- 0 until tableSize if Position(x, y).isLeftDiagonal
      } yield table(x)(y)).filter(_ != 0)
    } else if (pos.isRightDiagonal) {
      (for {
        x <- 0 until tableSize
        y <- 0 until tableSize if Position(x, y).isRightDiagonal
      } yield table(x)(y)).filter(_ != 0)
    } else Seq.empty

  private def getPossibleDigits(pos: Position): List[Int] = {
    val squareByX = pos.x / squareSize
    val squareByY = pos.y / squareSize

    val byY = (0 until tableSize).map(table(pos.y)(_)).filter(_ != 0)
    val byX = (0 until tableSize).map(table(_)(pos.x)).filter(_ != 0)
    val bySquare = for {
      x <- squareSize * squareByX until squareSize * (squareByX + 1)
      y <- squareSize * squareByY until squareSize * (squareByY + 1)
    } yield table(y)(x)

    val possibleDigits = (1 to tableSize).diff(byX).diff(byY).diff(bySquare)
    if (isDiagonal) {
      val byDiagonal = checkDiagonal(pos)
      possibleDigits.diff(byDiagonal).toList
    } else possibleDigits.toList

  }

  private def insertValue(pos: Position, digitToSet: Int): Seq[Seq[Int]] =
    table.updated(pos.y, table(pos.y).updated(pos.x, digitToSet))

  @tailrec
  private def findFirstSolution(possibleDigits: List[Int], pos: Position): Option[SudokuInner] = {
    possibleDigits match {
      case digit :: tail =>
        val mbSolution = copy(table = insertValue(pos, digit)).solve
        if (mbSolution.isDefined) mbSolution else findFirstSolution(tail, pos)
      case Nil => None
    }
  }

  // can be optimized
  private lazy val isBruteForceApplicable: Boolean = table.flatten.count(_ == 0) > sensitive

  lazy val solve: Option[SudokuInner] = {
    @tailrec
    def smartFill(cellIndex: Int = 0): Option[SudokuInner] =
      (cellIndex / tableSize, cellIndex % tableSize) match {
        case (9, _)                     => bruteForce()
        case (y, x) if table(y)(x) != 0 => smartFill(cellIndex + 1)
        case (y, x) =>
          val pos = Position(x, y)
          val possibleDigits = getPossibleDigits(pos)
          possibleDigits match {
            case digit :: Nil => copy(table = insertValue(pos, digit)).solve
            case _            => smartFill(cellIndex + 1)
          }
      }

    @tailrec
    def bruteForce(cellIndex: Int = 0): Option[SudokuInner] =
      (cellIndex / tableSize, cellIndex % tableSize) match {
        case (9, _)                     => Some(this)
        case (y, x) if table(y)(x) != 0 => bruteForce(cellIndex + 1)
        case (y, x) =>
          val pos = Position(x, y)
          val possibleDigits = getPossibleDigits(pos)
          findFirstSolution(possibleDigits, pos)
      }

    if (isBruteForceApplicable) bruteForce() else smartFill()
  }

  def prettyPrint: String = table.map(_.mkString(" ")).mkString("\n")
}

object SudokuInner {
  final val tableSize = 9
  final val squareSize = 3

  case class Position(x: Int, y: Int) {
    def isLeftDiagonal: Boolean = x == y
    def isRightDiagonal: Boolean = x + y == (tableSize - 1)
  }

  def rowFromString(line: String, missedValue: Char = '-'): Seq[Int] =
    line.toVector.map {
      case element if element == missedValue => 0
      case element if element.isDigit        => element.asDigit
    }

  def fromStdIn: SudokuInner =
    SudokuInner((1 to tableSize).map(_ => StdIn.readLine()).map(rowFromString(_)))
}
