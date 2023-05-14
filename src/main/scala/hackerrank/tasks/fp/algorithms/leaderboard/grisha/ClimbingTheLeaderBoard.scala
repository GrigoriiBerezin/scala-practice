package hackerrank.tasks.fp.algorithms.leaderboard.grisha

import scala.collection.immutable.TreeSet
import scala.io.StdIn

object ClimbingTheLeaderBoard extends App {
  case class PlayerPlacesWithRanked(places: Seq[Int], ranked: TreeSet[Int])

  def calculatePlayersPlaces(ranked: Seq[Int], players: Seq[Int]): Seq[Int] = {
    val rankedDist = ranked.distinct
    players.map(player => rankedDist.search(player)(Ordering.Int.reverse).insertionPoint + 1)
  }

  StdIn.readInt()
  val ranked = StdIn.readLine().split(" ").map(_.toInt)
  StdIn.readInt()
  val players = StdIn.readLine().split(" ").map(_.toInt)

  val places = calculatePlayersPlaces(ranked, players)

  places.foreach(println(_))
}
