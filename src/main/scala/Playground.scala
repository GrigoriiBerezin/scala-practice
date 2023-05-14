import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Playground extends App {
  val ids: Seq[Int] = Seq()

  def calculate(id: Int): Future[Int] = ???

  val f = Future.successful(Seq.empty[Int])
  val batches: Iterator[Seq[Int]] = ids.sliding(10)
  private val result: Future[Seq[Int]] = batches.foldLeft(f)((result, batch) => result.flatMap(acc => Future.sequence(batch.map(calculate)).map(futAcc => futAcc ++ acc)))
  ids.sliding(10).map(ids => Future.sequence(ids.map(calculate)))
}
