package assessments.others

object QuickSort extends App {

  def calculate(array: Vector[Int]): Vector[Int] =
    if (array.size <= 1) array
    else {
      val pivot = array(array.size / 2)
      Vector.concat(
        calculate(array filter (pivot > _)),
        array.filter(_ == pivot),
        calculate(array filter (pivot < _))
      )
    }

  println(calculate(Vector(102, 15, 6, 12, -5, 10, 56, 78, 11)))

}
