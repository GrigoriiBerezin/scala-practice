package assessments.alpha

import zio._

object AlphaBank extends ZIOAppDefault {
  case class Deal(price: Double, time: Long)

  case class Candle(start: Double, end: Double, low: Double, high: Double, time: Long)

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
}
