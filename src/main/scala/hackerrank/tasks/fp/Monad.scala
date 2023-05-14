package hackerrank.tasks.fp

trait Monad[T] {
  def flatMap[U](f: T => Monad[U]): Monad[U]

  def map[U](f: T => U): Monad[U] = flatMap(value => Monad.unit(f(value)))
}

object Monad {
  def unit[T](x: T): Monad[T] = ???
}

object Laws extends App {
  val monad = Some(4)
  val square: Int => Option[Int] = x => Some(x * 2)
  assert(monad.flatMap(square) == square(4)) // left law
  assert(monad.flatMap(Some(_)) == monad) // right law
  assert(monad.flatMap(square).flatMap(square) == monad.flatMap(x => square(x).flatMap(square))) // associativity
}
