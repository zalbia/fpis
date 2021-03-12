package part1.chapter3

sealed trait List[+A] { self =>
  def unsafeFoldRight[B](z: B)(f: (A, B) => B): B = self match {
    case Nil => z
    case Cons(x, xs) =>f(x, xs.unsafeFoldRight(z)(f))
  }

  def iterator: Iterator[A] = new Iterator[A] {
    var list = self

    def hasNext: Boolean = list match {
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
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ints: List[Int]): Int =
    ints.unsafeFoldRight(0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ds :List[Double]): Double =
    ds.unsafeFoldRight(1.0)(_ * _)
}
