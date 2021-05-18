package hackerrank.tasks.fp.general.sudoku.grisha

import hackerrank.tasks.fp.general.sudoku.SudokuSolver

class SudokuSolverImpl extends SudokuSolver {

  import SudokuInner.rowFromString

  override def solve(input: Seq[String]): Seq[String] = SudokuInner(input.map(rowFromString(_))).solve
    .map(_.table.map(_.mkString("")))
    .getOrElse(Seq.empty)
}
