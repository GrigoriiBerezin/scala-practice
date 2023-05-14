package hackerrank.tasks.fp

sealed trait MyList[+A] {

  def foldRight[B](acc: B)(f: (A, B) => B): B = this match {
    case MyNil => acc
    case MyCons(head, tail) => f(head, tail.foldRight(acc)(f))
  }

  def concat[B >: A](that: MyList[B]): MyList[B] = this match {
    case MyNil => that
    case MyCons(head, tail) => MyCons(head, tail.foldRight(that)(MyCons(_, _)))
  }

  @inline def :::[B >: A](that: MyList[B]): MyList[B] = concat(that)

  def map[B](f: A => B): MyList[B] = foldRight(MyNil: MyList[B])((a, b) => MyCons(f(a), b))

  def flatMap[B](f: A => MyList[B]): MyList[B] =
    foldRight(MyNil: MyList[B])((a, b) => f(a).concat(b))

  def filter(f: A => Boolean): MyList[A] =
    foldRight(MyNil: MyList[A])((a, b) => if (f(a)) MyCons(a, b) else b)

  def remove[B >: A](element: B): MyList[B] = filter(_ != element)

  def prepend[B >: A](element: B): MyList[B] = MyCons(element, this)

  @inline def ::[B >: A](element: B): MyList[B] = prepend(element)

  def append[B >: A](element: B): MyList[B] = foldRight(MyList(element))(MyCons(_, _))

  @inline def :+[B >: A](element: B): MyList[B] = append(element)

  override def toString: String = foldRight("")((a, b) => s"$a, $b")
}

case object MyNil extends MyList[Nothing]
final case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def apply[A](list: A*): MyList[A] =
    if (list.isEmpty) MyNil else MyCons(list.head, apply(list.tail: _*))

  def empty[A]: MyList[A] = MyList()

  class MyMonoid[A](a: A) {
    def map[B](f: A => B): MyMonoid[B] = flatMap(b => unit(f(b)))
    def unit[B](newA: B): MyMonoid[B] = ???

    def flatMap[B](f: A => MyMonoid[B]): MyMonoid[B] = ???
  }

}
