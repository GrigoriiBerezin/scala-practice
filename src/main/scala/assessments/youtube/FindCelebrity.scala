package assessments.youtube

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object FindCelebrity extends App {

  case class Person(id: Int, private val knownPeople: HashSet[Int]) {
    def knows(person: Person): Boolean = knownPeople.contains(person.id)
  }

  def calculate(people: Vector[Person]): Option[Person] = {
    @tailrec
    def inner(left: Int, right: Int): Option[Person] =
      if (right < left) None
      else if (right == left) {
        val person = people(left)
        val (isKnowOther, isKnownByOther) = people.foldLeft((false, true)) {
          case ((isKnowOther, isKnownByOther), `person`) => (isKnowOther, isKnownByOther)
          case ((isKnowOther, isKnownByOther), otherPerson) =>
            (isKnowOther || person.knows(otherPerson), isKnownByOther && otherPerson.knows(person))
        }
        if (isKnowOther || !isKnownByOther) None else Some(person)
      } else {
        val leftPerson = people(left)
        val rightPerson = people(right)
        if (leftPerson.knows(rightPerson)) inner(left + 1, right) else inner(left, right - 1)
      }

    inner(0, people.size - 1)
  }

  val person1 = Person(1, HashSet(2, 3, 4, 5))
  val person2 = Person(2, HashSet(1, 4, 5))
  val person3 = Person(3, HashSet(2, 4, 5))
  val person4 = Person(4, HashSet.empty)
  val person5 = Person(5, HashSet(1, 4))

  val example = Vector(person1, person2, person3, person4, person5)

  println(calculate(example))
}
