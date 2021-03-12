package part1.chapter2.ex2

import scala.annotation.tailrec

object IsSorted {
  def isSortedMerge[A](a: Array[A], ordered: (A, A) => Boolean): Boolean = a match {
    case Array() => true
    case Array(_) => true
    case Array(a, b) => ordered(a, b)
    case a: Array[A] =>
      val (left, right) = a.splitAt(a.length / 2)
      isSortedMerge(left, ordered) && isSortedMerge(right, ordered)
  }

  def isSorted[A](a: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val get = a.lift
    @tailrec
    def loop(i: Int, j: Int, acc: Boolean): Boolean = {
      if (j < a.length)
        get(i).zip(get(j)) match {
          case Some((a, b)) => loop(i + 1, j + 1, ordered(a, b) && acc)
          case None => acc
      } else acc
    }
    loop(0, 1, acc = true)
  }

  def main(args: Array[String]): Unit = {
    val orderedInt = (_: Int) <= (_: Int)
    List(
      Array(),
      Array(1),
      Array(1, 1),
      Array(1, 2),
      Array(1, 3),
      Array(2, 1),
      Array(1, 2, 3),
      Array(1, 3, 2)
    ).map(isSorted(_, orderedInt)).foreach(println)
  }
}
