package assessments.yandex.prepare

import scala.io.StdIn.readInt

object TheLongestArrayOfElem extends App {
  val n = readInt()

  def calculate(elements: Int): Int = {
    @annotation.tailrec
    def inner(currentLength: Int, maxLength: Int, elementsLeft: Int): Int = {
      if (elementsLeft <= 0) {
        Math.max(currentLength, maxLength)
      }
      else {
        val elem = readInt()
        if (elem == 1) inner(currentLength + 1, maxLength, elementsLeft - 1)
        else inner(0, Math.max(maxLength, currentLength), elementsLeft - 1)
      }
    }

    inner(0, 0, elements)
  }

  println(calculate(n))
}
