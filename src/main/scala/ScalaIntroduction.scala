import zio._

import scala.language.postfixOps

object ScalaIntroduction extends ZIOAppDefault {
  /**
   * На вход List[IO[String]]
   * Получить IO[(List[String], List[Throwable]) - результат агрегации выполненых IO и исключений
   */



    val talk = List(
      Clock.sleep(1 seconds) *> ZIO.succeed("red"),
      ZIO.fail(new RuntimeException("exception1")),
      ZIO.succeed("blue"),
      ZIO.fail(new RuntimeException("exception2")),
      ZIO.succeed("green"),
      ZIO.fail(new RuntimeException("exception3"))
    )

    // получить IO[(List[Throwable], List[String])]
    val resultIO: UIO[(List[RuntimeException], List[String])] =
      talk.map(effect => effect.either).foldRight(ZIO.succeed((List.empty[RuntimeException], List.empty[String]))) {
        case (effect, acc) => effect.flatMap {
          case Left(error) => acc.map {
            case (errors, values) => (error :: errors, values)
          }
          case Right(value) => acc.map {
            case (errors, values) => (errors, value :: values)
          }
        }
      }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = resultIO.debug.exitCode
}
