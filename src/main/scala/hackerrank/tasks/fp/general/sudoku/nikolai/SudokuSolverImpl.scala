package hackerrank.tasks.fp.general.sudoku.nikolai

import scala.annotation.tailrec
import scala.collection.Set
import hackerrank.tasks.fp.general.sudoku.SudokuSolver

class SudokuSolverImpl extends SudokuSolver {

  override def solve(input: Seq[String]): Seq[String] = {
    val preparedBoard = State.initializeState(input)
    val answer = SudokuEngine.fillSudoku(preparedBoard)
    answer.field.sliding(9, 9).map(row => row.mkString("")).toList
  }
}

object SudokuEngine {
  def fillSudoku(initState: State): State = {
    @tailrec
    def loop(currentState: State, listOfStates: List[State]): State = {

      if (currentState.isSolved) currentState

      else {
        val currentIndex = currentState.findIndexWithMinCandidates
        val currentCandidates = currentState.candidates(currentIndex)

        if (currentCandidates.isEmpty && listOfStates.isEmpty) State(Vector(), Vector())

        else if (currentCandidates.isEmpty) {
          loop(listOfStates.head, listOfStates.tail)

        } else if (currentCandidates.size == 1) {
          val newState = currentState.updateStateByNewCellValue(currentIndex, currentCandidates.head)
          loop(newState, listOfStates)

        } else {
          val listOfCandidatesStates: List[State] = State.getListOfAllCandidatesStates(currentState, currentIndex)
          loop(listOfCandidatesStates.head, listOfCandidatesStates.tail ++ listOfStates)
        }
      }
    }

    loop(initState, List())
  }
}

class State(val field: Vector[Int], val candidates: Vector[Set[Int]]) {

  def findIndexWithMinCandidates: Int = {
    candidates.zipWithIndex.filter {
      case (_, index) => field(index) == 0
    }.minBy {
      case (candidates, _) => candidates.size
    }._2
  }

  def updateStateByNewCellValue(index: Int, newValue: Int): State = {
    val stateWithFieldUpdated = updateField(index, newValue)
    val newCandidates = stateWithFieldUpdated.updateCandidates(index)
    State(stateWithFieldUpdated.field, newCandidates)
  }

  def updateCandidates(initIndex: Int): Vector[Set[Int]] = {
    val commonGroupIndices = (0 to 80).filter(num => State.isCellInOneSudokuGroup(initIndex, num)).toSet

    candidates.zipWithIndex.map {
      case (cellCandidates, index) if commonGroupIndices.contains(index) => cellCandidates - field(initIndex)
      case (cellCandidates, _) => cellCandidates
    }
  }

  def updateField(index: Int, newValue: Int): State = {
    val newField = field.updated(index, newValue)
    State(newField, candidates)
  }

  def isSolved: Boolean = {
    !field.contains(0)
  }

  override def toString: String = {
    field.sliding(9, 9).map(row => row.mkString(" ")).mkString("\n")
  }
}

object State {

  val allDigitSet: Set[Int] = (1 to 9).toSet

  def apply(field: Vector[Int], candidates: Vector[Set[Int]]) = new State(field, candidates)

  def getListOfAllCandidatesStates(state: State, index: Int): List[State] = {
    @tailrec
    def loop(candidates: Set[Int], acc: List[State]): List[State] = {
      if (candidates.isEmpty) acc
      else {
        val newState = state.updateStateByNewCellValue(index, candidates.head)
        loop(candidates.tail, newState +: acc)
      }
    }

    loop(state.candidates(index), List[State]())
  }

  def initializeState(inputList: Seq[String]): State = {

    def findCellCandidates(field: Vector[Int], targetIndex: Int): Set[Int] = {
      allDigitSet diff field.zipWithIndex
        .filter { case (_, currentIndex) => isCellInOneSudokuGroup(currentIndex, targetIndex) }
        .map { case (value, _) => value }.toSet
    }

    val initValues = inputList.flatten.map {
      case '-' => 0
      case ch => ch.asDigit
    }.toVector

    val initCandidates: Vector[Set[Int]] = initValues.zipWithIndex
      .map { case (_, index) => findCellCandidates(initValues, index) }

    State(initValues, initCandidates)
  }

  def isCellInOneSudokuGroup(initIndex: Int, targetIndex: Int): Boolean = {
    def getRow(index: Int): Int = index / 9 + 1
    def getColumn(index: Int): Int = index % 9 + 1
    def getBlock(index: Int): Int = ((Math.floor((index % 9) / 3) + 3 * Math.floor(index / (9 * 3))) + 1).toInt

    getRow(initIndex) == getRow(targetIndex) ||
      getColumn(initIndex) == getColumn(targetIndex) ||
      getBlock(initIndex) == getBlock(targetIndex)
  }

}
