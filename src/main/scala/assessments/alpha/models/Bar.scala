package assessments.alpha.models

case class Bar(start: Double, end: Double, low: Double, high: Double, time: Long)

object Bar {
  def byOneDeal(deal: Deal): Bar = {
    val price = deal.price
    Bar(price, price, price, price, deal.time)
  }
}
