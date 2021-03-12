package part1.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def iterator[A](as: List[A]): Iterator[A] = new Iterator[A] {
    var list = as

    def hasNext: Boolean = as match {
      case Nil => false
      case _ => true
    }

    def next(): A = list match {
      case Nil => throw new NoSuchElementException("Calling next on an empty iterator")
      case Cons(head, tail) =>
        list = tail
        head
    }
  }
}
