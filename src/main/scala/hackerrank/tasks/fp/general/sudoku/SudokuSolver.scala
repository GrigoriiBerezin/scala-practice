package hackerrank.tasks.fp.general.sudoku

trait SudokuSolver {
  def solve(input: Seq[String]): Seq[String]
}

object SudokuSolver {
  // this solver doesn't actually solve anything
  // we need it only to make everything compile
  val Dummy: SudokuSolver = identity

  val Grisha: SudokuSolver = new grisha.SudokuSolverImpl

  val NikitaM: SudokuSolver = new nikitam.SudokuSolverImpl

  val Nikolai: SudokuSolver = new nikolai.SudokuSolverImpl
}