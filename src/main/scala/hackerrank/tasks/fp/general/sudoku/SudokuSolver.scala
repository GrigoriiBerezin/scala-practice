package hackerrank.tasks.fp.general.sudoku

trait SudokuSolver {
  def solve(input: Seq[String]): Seq[String]
}

object SudokuSolver {
  // this solver that doesn't actually solve anything
  // we need it only to make everything compile
  val Dummy: SudokuSolver = identity
}