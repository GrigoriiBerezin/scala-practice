package hackerrank.tasks.fp.algorithms.tinkoff.grisha

import scala.io.StdIn

object Tinkoff extends App {
  /* Во время разработки некоторой задачи Саша решил сгенерировать несколько новых тестов. Каждый
   * тест Саши должен представлять собой натуральное число, не меньшее l и не большее r. Кроме того,
   * натуральное число в тесте обязательно должно состоять из одинаковых цифр.
   *
   * Например, число 999 подходит под это требование, а число 123 — нет. Какое максимальное число
   * различных тестов сможет создать Саша?
   *
   * Формат входных данных
   *
   * В единственной строке вводятся два натуральных числа l, r (1≤l,r≤10^18)— ограничения на тесты
   * Саши.
   *
   * Обратите внимания, что эти числа не вместятся в 32-битный тип данных, используйте 64-битный при
   * необходимости
   *
   * Формат выходных данных
   *
   * Выведите одно число — количество тестов, которое может сделать Саша. */

  def countTests(upperBound: BigInt): Int = {
    val stringBound = upperBound.toString()
    val degree = stringBound.length - 1

    val countSimpleTests = 9 * degree
    stringBound.toList match {
      case ::(lead, tail) =>
        val leadDigit = lead.asDigit
        val level = if (tail.forall(c => c.asDigit >= leadDigit)) 0 else 1
        (leadDigit - level) + countSimpleTests
      case Nil => countSimpleTests
    }
  }

  def calculateTests(upperBound: BigInt): Seq[BigInt] = {
    val degree = upperBound.toString().length - 1
    val simpleTests = for {
      d <- 1 to degree
      digit <- 1 to 9
    } yield BigInt(digit.toString * d)
    val hardTests = upperBound.toString().toList match {
      case ::(lead, tail) =>
        val leadDigit = lead.asDigit
        val level = if (tail.forall(c => c.asDigit >= leadDigit)) 0 else 1
        for { d <- 1 to (leadDigit - level) } yield BigInt(d.toString * (degree + 1))
      case Nil => Seq.empty
    }
    simpleTests ++ hardTests
  }

  val r = BigInt(StdIn.readLine().trim)

  val result = calculateTests(r).length
  val result2 = countTests(r)
  print(result, result2)
}
