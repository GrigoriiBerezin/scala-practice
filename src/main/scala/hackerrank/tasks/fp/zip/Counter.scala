package hackerrank.tasks.fp.zip

class Counter(text: String) extends Map[Char, Int] {

  private val map = text
    .foldLeft(Map.empty[Char, Int])((map, char) => map + (char -> (map.getOrElse(char, 0) + 1)))

  override def removed(key: Char): Map[Char, Int] = map.removed(key)

  override def updated[V1 >: Int](key: Char, value: V1): Map[Char, V1] = map.updated(key, value)

  override def get(key: Char): Option[Int] = map.get(key)

  override def iterator: Iterator[(Char, Int)] = map.iterator
}

object Counter {
  def apply(text: String) = new Counter(text)
}
