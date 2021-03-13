package part1.chapter5

import scala.annotation.tailrec

sealed trait LazyList[+A] {
  self =>

  import LazyList.{Cons, Empty, cons, unfold}

  final def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  /** exercise 5.1 */
  final def toList: List[A] = self match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /** exercise 5.2 */
  final def take(n: Int): LazyList[A] = self match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) cons(h(), t().take(n - 1)) else Empty
  }

  /** exercise 5.3 */
  final def takeWhile(p: A => Boolean): LazyList[A] = self match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
  }

  @tailrec final def exists(p: A => Boolean): Boolean = self match {
    case Empty => false
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  final def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  final def existsFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /** exercise 5.4 */
  final def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /** exercise 5.5 */
  final def takeWhileFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  /** exercise 5.6 */
  final def headOptionFoldRight: Option[A] =
    foldRight(Option.empty[A])((a, b) => if (b.isEmpty) Some(a) else None)

  // exercise 5.7
  final def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => cons(f(a), b))

  final def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  final def append[B >: A](b: => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, tail) => tail match {
      case Empty => cons(a, cons(b, tail))
      case tail: Cons[A] => cons(a, tail)
    })

  @tailrec final def foldLeft[B](z: => B)(f: (B, => A) => B): B = self match {
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(z, h()))(f)
  }

  final def reverse: LazyList[A] =
    foldLeft(LazyList.empty[A])((tail, a) => cons(a, tail))

  final def concat[B >: A](that: => LazyList[B]): LazyList[B] =
    self.reverse.foldLeft(that)((b, a) => cons(a, b))

  final def ++[B >: A](that: => LazyList[B]): LazyList[B] =
    concat(that)

  final def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => f(a) ++ b)

  // end of exercise 5.7

  @tailrec final def foreach(f: A => Unit): Unit = self match {
    case Empty => ()
    case Cons(h, t) =>
      f(h())
      t().foreach(f)
  }

  /** exercise 5.13 */
  object UseUnfold {
    def map[B](f: A => B): LazyList[B] =
      unfold(self) {
        case Empty => None
        case Cons(h, t) => Some(f(h()), t())
      }

    def take(n: Int): LazyList[A] =
      unfold((self, n)) { case (as, n) => as match {
        case Empty => None
        case Cons(h, t) => if (n > 0) Some(h(), (t(), n - 1)) else None
      }
      }

    def takeWhile(p: A => Boolean): LazyList[A] =
      unfold(self) {
        case Empty => None
        case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      }

    def zipWith[B, C](b: LazyList[B])(f: (A, B) => C): LazyList[C] =
      unfold((self, b)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }

    def zipAll[B](b: LazyList[B]): LazyList[(Option[A], Option[B])] =
      unfold((self, b)) {
        case (Empty, Empty) => None
        case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
        case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      }
  }

  lazy val length: Int = foldRight(0)((_, len) => len + 1)

  def startsWith[B >: A](b: LazyList[B]): Boolean =
    length >= b.length && UseUnfold.zipWith(b)(_ == _).forAll(identity)

  def tails: LazyList[LazyList[A]] =
    unfold(self) {
      case Empty => None
      case as @ Cons(_, t) => Some(as, t())
    }

  def hasSubsequence[B >: A](b: LazyList[B]): Boolean =
    tails.exists(_.startsWith(b))

  /**
   * exercise 5.16
   *
   * pretty sure this is O(n)
   * T(n) ~= 3n; 2n in reverse and n for unfold.
   */
  def scanRight[B >: A](z: B)(f: (B, B) => B): LazyList[B] =
    unfold((cons(z, self.reverse), z)) { // self.reverse is O(n)
      case (Empty, _) => None
      case (Cons(h, t), b) =>
        val b1 = f(b, h())
        Some(b1, (t(), b1))
    }.reverse // unfold...reverse is O(n)
}

object LazyList {

  case object Empty extends LazyList[Nothing]

  case class Cons[+A] private(h: () => A, t: () => LazyList[A]) extends LazyList[A]

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /** exercise 5.8 */
  def constant[A](a: A): LazyList[A] = {
    lazy val as: LazyList[A] = cons(a, as)
    as
  }

  /** exercise 5.9 */
  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  /** exercise 5.10 */
  def fibs: LazyList[Int] = {
    def loop(a: Int, b: Int): LazyList[Int] =
      cons(a, loop(b, a + b))

    loop(0, 1)
  }

  /** exercise 5.11 */
  def unfold[A, S](z: => S)(f: S => Option[(A, S)]): LazyList[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  /** exercise 5.12 */
  object Unfold {
    def fibs: LazyList[Int] =
      unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

    def from(n: Int): LazyList[Int] =
      unfold(n)(n => Some((n, n + 1)))

    def constant[A](a: A): LazyList[A] =
      unfold(a)(a => Some(a, a))

    def ones: LazyList[Int] =
      unfold(())(_ => Some(1, ()))
  }

}
