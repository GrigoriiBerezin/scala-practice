package sorting

import org.scalacheck.Gen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sorting.cocktailshakesort.CocktailShakerSort
import sorting.insertionsort.InsertionSort
import sorting.selectionsort.SelectionSort

class SortTest extends AnyWordSpec with ScalaCheckPropertyChecks {
  private val numberGen = Gen.choose(-100, 100)
  private val listOfNumbers = Gen.listOf(numberGen)

  private val sortAlgorithms: Seq[SortAlgorithm] = Seq(InsertionSort, CocktailShakerSort, SelectionSort)

  def testSort(algorithm: SortAlgorithm): Unit = s"sort the list by ${algorithm.toString} algorithm" in {
    forAll(listOfNumbers) { list =>
      val sortedList = algorithm.sort(list)
      assert(sortedList == list.sorted)
    }
  }

  "A sorting algorithm" should {
    sortAlgorithms.foreach(testSort)
  }

}
