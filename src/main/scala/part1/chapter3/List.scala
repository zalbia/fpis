package part1.chapter3

import scala.annotation.tailrec

sealed trait List[+A] {
  self =>

  import List.{Cons, Nil}

  def unsafeFoldRight[B](z: B)(f: (A, B) => B): B = self match {
    case Nil => z
    case Cons(x, xs) => f(x, xs.unsafeFoldRight(z)(f))
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

  /** exercise 3.2 */
  def tailOption: Option[List[A]] = self match {
    case Nil => None
    case Cons(_, tail) => Some(tail)
  }

  /** exercise 3.3 */
  def setHead[B >: A](a: B): List[B] = self match {
    case Nil => Cons(a, Nil)
    case Cons(_, tail) => Cons(a, tail)
  }

  /** exercise 3.4 */
  def drop(n: Int): List[A] = self match {
    case Nil => Nil
    case list@Cons(_, tail) =>
      if (n == 0) list
      else if (n == 1) tail
      else tail.drop(n - 1)
  }

  /** exercise 3.5 */
  def dropWhile(f: A => Boolean): List[A] = self match {
    case Nil => Nil
    case list@Cons(a, tail) =>
      if (f(a)) tail.dropWhile(f)
      else list
  }

  /** exercise 3.6 */
  def init: List[A] = self match {
    case Nil => Nil
    case Cons(a, tail) =>
      tail match {
        case Nil => Nil
        case _: Cons[A] => Cons(a, tail.init)
      }
  }

  /**
   * exercise 3.7
   *
   * No, `foldRight` doesn't early exit. You could use some other combinators
   * to simulate an early exit, or write another foldRight variant
   */
  def foldRightEarlyExit[B](z: B, exit: B)(f: (A, B) => B)(exitPred: A => Boolean): B = self match {
    case Nil => z
    case Cons(x, xs) =>
      if (exitPred(x)) exit
      else f(x, xs.foldRightEarlyExit(z, exit)(f)(exitPred))
  }

  def productEarlyExit[B >: A](implicit ev: B =:= Double): Double =
    foldRightEarlyExit(1.0, exit = 0.0)(_ * _)(ev(_) == 0.0)

  /** exercise 3.9 */
  def length: Int = self.unsafeFoldRight(0)((_, i) => i + 1)

  /** exercise 3.10 */
  def foldLeft[B](z: B)(f: (B, A) => B): B = self match {
    case Nil => z
    case Cons(x, xs) => xs.foldLeft(f(z, x))(f)
  }

  /** exercise 3.11 */
  def leftSum[B >: A](implicit ev: Numeric[B]): B =
    foldLeft(implicitly[Numeric[B]].zero)(implicitly[Numeric[B]].plus)

  def leftProduct[B >: A](implicit ev: Numeric[B]): B =
    foldLeft(implicitly[Numeric[B]].one)(implicitly[Numeric[B]].times)

  def leftLength: Int =
    foldLeft(0)((n, _) => n + 1)

  /** exercise 3.12 */
  def reverse: List[A] =
    foldLeft(Nil: List[A])((b, a) => Cons(a, b))

  /** exercise 3.13 */
  def foldRight[B](z: B)(f: (A, B) => B): B =
    self.reverse.foldLeft(z)((b, a) => f(a, b))

  /** exercise 3.14 */
  def append[B >: A](that: List[B]): List[B] =
    self.reverse.foldLeft(that)((b, a) => Cons(a, b))

  /** exercise 3.16 */
  def increment[B >: A](implicit ev: Numeric[B]): List[B] =
    self.foldRight(Nil: List[B])((a, b) =>
      Cons(implicitly[Numeric[B]].plus(a, implicitly[Numeric[B]].one), b))

  /** exercise 3.17 */
  def doublesToString[B >: A](implicit ev: B =:= Double): List[String] =
    self.foldRight(Nil: List[String])((a, b) => Cons(ev(a).toString, b))

  /** exercise 3.18 */
  def map[B](f: A => B): List[B] =
    self.foldRight(Nil: List[B])((a, b) => Cons(f(a), b))

  /** exercise 3.19 */
  def filter(f: A => Boolean): List[A] =
    self.foldRight(Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def removeOdd[B >: A](implicit int: Integral[B]): List[B] =
    self.filter(a => {
      int.rem(a, int.fromInt(2)) == int.zero
    })

  /** exercise 3.20 */
  def flatMap[B](f: A => List[B]): List[B] =
    self.foldRight(Nil: List[B])((a, b) => f(a).append(b))

  /** exercise 3.21 */
  def flatMapFilter(p: A => Boolean): List[A] =
    self.flatMap(a => if (p(a)) List(a) else Nil)

  /** exercise 3.22 */
  def zipAdd[B >: A](that: List[B])(implicit numeric: Numeric[B]): List[B] = {
    @tailrec def go(a: List[B], b: List[B], acc: List[B]): List[B] =
      (a, b) match {
        case (Cons(a, as), Cons(b, bs)) => go(as, bs, Cons(numeric.plus(a, b), acc))
        case _ => acc
      }

    go(self, that, Nil)
  }

  /** exercise 3.23 */
  def zipWith[B >: A, C](that: List[B])(f: (B, B) => C): List[C] = {
    @tailrec def go(a: List[B], b: List[B], acc: List[C]): List[C] =
      (a, b) match {
        case (Cons(a, as), Cons(b, bs)) => go(as, bs, Cons(f(a, b), acc))
        case _ => acc
      }

    go(self, that, Nil)
  }

  /** exercise 3.24 */
  def hasSubsequence[B >: A](sub: List[B]): Boolean = sub match {
    case Nil => false
    case Cons(head, _) =>
      self.dropWhile(_ != head).zipWith(sub)(_ == _).foldLeft(true)(_ && _)
  }
}

object List {

  case object Nil extends List[Nothing]

  case class Cons[A](head: A, tail: List[A]) extends List[A]

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

  def product2(ds: List[Double]): Double =
    ds.unsafeFoldRight(1.0)(_ * _)

  /** exercise 3.1 */
  def exercise1: Int = List(1, 2, 3, 4, 5) match {
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /**
   * exercise 3.8
   *
   * foldRight and cons yield the same structure
   */
  def exercise8: List[Int] =
    List(1, 2, 3).unsafeFoldRight(Nil: List[Int])(Cons(_, _))

  /** exercise 3.15 */
  def concatenate[A](lists: List[List[A]]): List[A] =
    lists.foldRight(Nil: List[A])(_ append _)
}
