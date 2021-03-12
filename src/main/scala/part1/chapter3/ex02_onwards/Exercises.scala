package part1.chapter3.ex02_onwards

import part1.chapter3._

object Exercises {
  implicit class ListOps[A](list: List[A]) {
    /** exercise 2 */
    def tailOption: Option[List[A]] = list match {
      case Nil => None
      case Cons(_, tail) => Some(tail)
    }

    /** exercise 3 */
    def setHead(a: A): List[A] = list match {
      case Nil => Cons(a, Nil)
      case Cons(_, tail) => Cons(a, tail)
    }

    /** exercise 4 */
    def drop(n: Int): List[A] = list match {
      case Nil => Nil
      case list @ Cons(_, tail) =>
        if (n == 0) list
        else if (n == 1) tail
        else tail.drop(n - 1)
    }

    /** exercise 5 */
    def dropWhile(f: A => Boolean): List[A] = list match {
      case Nil => Nil
      case list @ Cons(a, tail) =>
        if (f(a)) tail.dropWhile(f)
        else list
    }

    /** exercise 6 */
    def init: List[A] = list match {
      case Nil => Nil
      case l @ Cons(a, tail) =>
        tail match {
          case Nil => Nil
          case _ : Cons[A] => Cons(a, tail.init)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    println("tail")
    Array(
      List(),
      List(1),
      List(1, 2)
    ).map(_.tailOption).foreach(println)

    println("\nsetHead")
    Array(
      (List(), 1),
      (List(1), 2),
      (List(1, 2), 3)
    ).map { case (list, a) => list.setHead(a) }.foreach(println)

    println("\ndrop")
    Array(
      (List(), 1),
      (List(1), 2),
      (List(1, 2), 1),
      (List(1, 2), 2)
    ).map { case (list, a) => list.drop(a) }.foreach(println)

    println("\ndropWhile")
    val lessThan5: Int => Boolean = _ < 5
    Array(
      (List(), lessThan5),
      (List(1), lessThan5),
      (List(1, 2), lessThan5),
      (List(1, 2), lessThan5),
      (List(1, 2, 5, 1, 2), lessThan5)
    ).map { case (list, a) => list.dropWhile(a) }.foreach(println)

    println("\ninit")
    Array(
      List(),
      List(1),
      List(1, 2),
      List(1, 2),
      List(1, 2, 5, 1, 2)
    ).map(_.init).foreach(println)
  }
}
