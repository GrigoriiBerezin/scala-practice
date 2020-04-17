package hackerrank.tasks.fp.functionalstructures.prisontransport.nikitam

import scala.io.StdIn.{readInt, readLine}

object Solution {

  def main(args: Array[String]): Unit = {
    val n = readInt
    val m = readInt
    val linksStr = List.fill(m)(readLine)

    val links = linksStr.map(parseLink)

    val prisonerIds = (1 to n).map(Id(_))
    val idToLinkedIds = linkAllPrisoners(links, prisonerIds)
    val allGroups = getAllGroups(idToLinkedIds)
    val amountOfBuses = getAmountOfBuses(allGroups)

    println(amountOfBuses)
  }

  private def linkAllPrisoners(allLinks: List[Link], prisonerIds: Seq[Id]): Map[Id, Links] = {

    def addLink(link: Link, idToLinkedIds: Map[Id, Links]): Map[Id, Links] = {
      val Link(firstPrisonerId, secondPrisonerId) = link

      val firstPrisonerLinks = idToLinkedIds(firstPrisonerId)
      val secondPrisonerLinks = idToLinkedIds(secondPrisonerId)

      val updatedFirstLinks = firstPrisonerLinks.copy(value = firstPrisonerLinks.value + secondPrisonerId)
      val updatedSecondLinks = secondPrisonerLinks.copy(value = secondPrisonerLinks.value + firstPrisonerId)

      idToLinkedIds
        .updated(firstPrisonerId, updatedFirstLinks)
        .updated(secondPrisonerId, updatedSecondLinks)
    }

    val idsWithoutLinks = prisonerIds.map(id => id -> Links.empty).toMap

    allLinks.foldLeft(idsWithoutLinks)((idToLinkedIds, link) => addLink(link, idToLinkedIds))
  }

  @scala.annotation.tailrec
  private def getAllGroups(prisoners: Map[Id, Links], groups: Set[Group] = Set.empty): Set[Group] = {

    def findGroupForId(id: Id, knownGroupPart: Group = Group.empty): Group = {
      val knownGroup = knownGroupPart.copy(members = knownGroupPart.members + id)
      val linkedIds = prisoners(id).value

      linkedIds.foldLeft(knownGroup) { case (group, linkedId) =>
        if (group.members.contains(linkedId)) group
        else findGroupForId(linkedId, group)
      }
    }

    prisoners.headOption match {
      case Some((id, _)) =>
        val newGroup = findGroupForId(id)
        getAllGroups(prisoners -- newGroup.members, groups + newGroup)
      case None =>
        groups
    }
  }

  private def getAmountOfBuses(groups: Set[Group]): Int =
    groups.toVector.map(_.members.size).map(sqrt).sum

  private def sqrt(int: Int): Int =
    Math.sqrt(int).ceil.toInt

  private def parseLink(linksStr: String): Link = {
    val Array(from, to) = linksStr.split(" ").map(_.toInt).map(Id(_))
    Link(from, to)
  }
}

case class Id(value: Long) extends AnyVal

case class Group(members: Set[Id])

object Group {
  val empty: Group = Group(Set.empty)
}

case class Link(firstPrisonerId: Id, secondPrisonerId: Id)

case class Links(value: Set[Id])

object Links {
  val empty: Links = Links(Set.empty)
}
