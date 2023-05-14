package hackerrank.tasks.fp.zip

import scala.annotation.tailrec

object Huffman extends App {

  private type Pair = (Char, Int)

  private def makeOrderedTreeList(freqs: List[Pair]): List[Tree[Char]] = freqs.map { case (char, freq) =>
    Tree.Leaf(char, freq)
  }.sortBy(Tree.weight)

  private def combine[A](trees: List[Tree[A]]): List[Tree[A]] = trees match {
    case head1 :: head2 :: tail => (Tree.merge(head1, head2) :: tail).sortBy(Tree.weight)
    case _ => trees
  }

  @tailrec private def until[A](pred: A => Boolean, f: A => A)(acc: A): A =
    if (pred(acc)) acc else until(pred, f)(f(acc))

  private def createHuffTree(text: String): Tree[Char] =
    until[List[Tree[Char]]](_.size == 1, combine)(makeOrderedTreeList(Counter(text).toList)).head

  def encode(text: String): List[Int] = {
    def encodeChar(tree: Tree[Char], char: Char): List[Int] = {
      require(Tree.value(tree).contains(char))
      tree match {
        case Tree.Branch(left, right, _, _) =>
          if (Tree.value(left).contains(char)) 0 :: encodeChar(left, char)
          else 1 :: encodeChar(right, char)
        case Tree.Leaf(_, _) => Nil
      }
    }

    def _encode(tree: Tree[Char], text: List[Char]): List[List[Int]] = text match {
      case char :: tail => encodeChar(tree, char) :: _encode(tree, tail)
      case Nil => Nil
    }

    val tree = createHuffTree(text)

    _encode(tree, text.toList).flatten
  }

  def decode(filename: String): String = ???
}
