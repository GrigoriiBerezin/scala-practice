package hackerrank.tasks.fp.functionalstructures.stringmingling.grisha

import zio._
import zio.Console._

object StringMingling extends ZIOAppDefault {
  private def stringMingling(p: String, q: String): UIO[String] =
    ZIO.succeed {
      p.zip(q).map { case (c1, c2) => s"$c1$c2" }.reduce(_ + _)
    }

  override def run: URIO[Any, ExitCode] =
    (for {
      p <- readLine
      q <- readLine
      result <- stringMingling(p, q)
      _ <- printLine(result)
    } yield ()).exitCode
}
