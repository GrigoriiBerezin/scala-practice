package hackerrank.tasks.fp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyListSuite extends AnyFlatSpec with Matchers {
  behavior of "apply"

  it should "build list from arguments" in {
    val expected = MyCons(1, MyCons(2, MyCons(3, MyCons( 4, MyNil))))

    val result = MyList(1, 2, 3, 4)

    result shouldBe expected
  }

  it should "build empty list when no arguments" in {
    val expected = MyNil

    val result = MyList()

    result shouldBe expected
  }

  behavior of "prepend(::)"

  it should "add element to the top of the list" in {
    val expected = MyList(4, 1, 2, 3)

    val result = 4 :: MyList(1, 2, 3)

    result shouldBe expected
  }

  ignore should "not add MyNil to the top of the list" in {
    val expected = MyList(MyNil, 1, 2, 3)

    val result = MyNil :: MyList(1, 2, 3)

    result shouldBe expected
  }

  behavior of "append(:+)"

  it should "add element to the end of the list" in {
    val expected = MyList(1, 2, 3, 4)

    val result = MyList(1, 2, 3) :+ 4

    result shouldBe expected
  }

  behavior of "toString"

  it should "return human-readable string of the list" in {
    val expected = "1, 2, 3, 4, "

    val result = MyList(1, 2, 3, 4).toString

    result shouldBe expected
  }

  it should "return comma whe the list is empty" in {
    val expected = ""

    val result = MyList().toString

    result shouldBe expected
  }

  behavior of "foldRight"

  it should "fold from left to the right on the list" in {
    val expected = -2

    val result = MyList(1, 2, 3, 4).foldRight(0)(_ - _)

    result shouldBe expected
  }

  it should "do nothing on empty list" in {
    val expected = 0

    val result = MyList.empty[Int].foldRight(0)(_ + _)

    result shouldBe expected
  }

  behavior of "concat(:::)"

  it should "concat two lists" in {
    val expected = MyList(1, 2, 3, 4)

    val result = MyList(1, 2).concat(MyList(3, 4))

    result shouldBe expected
  }

  behavior of "map"

  it should "map the list" in {
    val expected = MyList(2, 4, 8, 16)

    val result = MyList(1, 2, 3, 4).map(Math.pow(2, _))

    result shouldBe expected
  }

  behavior of "flatMap"

  it should "flatMap the list" in {
    val expected = MyList('a', 'b', 'b', 'c', 'c', 'd')

    val result = MyList("ab", "bc", "cd").flatMap(s => MyList(s.toSeq: _*))

    result shouldBe expected
  }

  behavior of "filter"

  it should "filter list" in {
    val expected = MyList(1, 2)

    val result = MyList(1, 2, 3, 4).filter(_ < 3)

    result shouldBe expected
  }

  behavior of "remove"

  it should "remove element from list" in {
    val expected = MyList("Don't delete me")

    val result = MyList("Error", "Don't delete me", "Error").remove("Error")

    result shouldBe expected
  }

  behavior of "covariant"

  sealed trait Foo
  case class Bar(b: Int) extends Foo
  case class Baz(z: Int) extends Foo

  it should "take the highest common parent" in {
    assertCompiles("val result: MyList[Foo] = Bar(1) :: Bar(2) :: Baz(3) :: MyNil")
  }

}
