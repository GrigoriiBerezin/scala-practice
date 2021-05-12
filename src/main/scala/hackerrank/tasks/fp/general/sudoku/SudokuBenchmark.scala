package hackerrank.tasks.fp.general.sudoku

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
class SudokuBenchmark {

  private val emptyBoard: Seq[String] = Vector(
    "---------",
    "---------",
    "---------",
    "---------",
    "---------",
    "---------",
    "---------",
    "---------",
    "---------"
  )

  private val easyBoard: Seq[String] = Vector(
    "6---79-32",
    "----6-5--",
    "2-9--87--",
    "9-63-5--1",
    "85----3--",
    "473--125-",
    "-4268-9--",
    "----13427",
    "-9-2--6--"
  )

  private val mediumBoard: Seq[String] = Vector(
    "2--6-9-3-",
    "984---6--",
    "-1---7--9",
    "1-6--3---",
    "8--4---5-",
    "-4-2--3--",
    "------41-",
    "--91-5763",
    "----7-9--"
  )

  private val hardBoard: Seq[String] = Vector(
    "-4-6---19",
    "--8----34",
    "----27---",
    "------69-",
    "-6--4----",
    "8---5---7",
    "-97--256-",
    "-2---8---",
    "58-------"
  )

  private val expertBoard: Seq[String] = Vector(
    "-2--81---",
    "-5----72-",
    "-7-5---9-",
    "---------",
    "--63--4--",
    "9----4--8",
    "----4----",
    "24--95---",
    "---8-3-5-"
  )

  // TODO set your solver here
  private val sudokuSolver: SudokuSolver = SudokuSolver.Dummy

  @Benchmark
  def fromEmptyBoard(): Unit = sudokuSolver.solve(emptyBoard)

  @Benchmark
  def fromEasyBoard(): Unit = sudokuSolver.solve(easyBoard)

  @Benchmark
  def fromMediumBoard(): Unit = sudokuSolver.solve(mediumBoard)

  @Benchmark
  def fromHardBoard(): Unit = sudokuSolver.solve(hardBoard)

  @Benchmark
  def fromExpertBoard(): Unit = sudokuSolver.solve(expertBoard)
}