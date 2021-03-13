package part1.chapter4

import scala.util.Try

sealed trait Option[+A] { self =>
  import Option.{ None, Some }

  // exercise 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = self match {
    case None => None
    case Some(a) => f(a)
  }

  def map[B](f: A => B): Option[B] =
    flatMap(a => Some(f(a)))

  def getOrElse[B >: A](default: => B): B = self match {
    case Some(get) => get
    case None => default
  }

  // throw in a fold for good measure
  def fold[B](z: B)(f: A => B): B =
    map(f).getOrElse(z)

  def orElse[B >: A](that: => Option[B]): Option[B] =
    fold(that)(Some(_))

  def filter(f: A => Boolean): Option[A] =
    map(f).flatMap(p => if (p) self else None)
  // end of exercise 4.1
}

object Option {
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /** exercise 4.2 */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /** exercise 4.3 */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  /** exercise 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List.empty[A]))(map2(_, _)(_ :: _))

  /** exercise 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(List.empty[B]))((a, b) => map2(f(a), b)(_ :: _))

  def traverseSequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}
