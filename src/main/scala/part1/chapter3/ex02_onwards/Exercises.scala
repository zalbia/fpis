package part1.chapter3.ex02_onwards

import part1.chapter3._

import scala.annotation.tailrec

object Exercises {

  implicit class ListOps[A](self: List[A]) {
    /** exercise 3.2 */
    def tailOption: Option[List[A]] = self match {
      case Nil => None
      case Cons(_, tail) => Some(tail)
    }

    /** exercise 3.3 */
    def setHead(a: A): List[A] = self match {
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

    def productEarlyExit[B](implicit ev: A =:= Double): Double =
      foldRightEarlyExit(1.0, exit = 0.0)(_ * _)(ev(_) == 0.0)

    /** exercise 3.9 */
    def length: Int = self.unsafeFoldRight(0)((_, i) => i + 1)

    /** exercise 3.10 */
    def foldLeft[B](z: B)(f: (B, A) => B): B = self match {
      case Nil => z
      case Cons(x, xs) => xs.foldLeft(f(z, x))(f)
    }

    /** exercise 3.11 */
    def leftSum(implicit ev: Numeric[A]): A =
      foldLeft(implicitly[Numeric[A]].zero)(implicitly[Numeric[A]].plus)

    def leftProduct(implicit ev: Numeric[A]): A =
      foldLeft(implicitly[Numeric[A]].one)(implicitly[Numeric[A]].times)

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
    def increment(implicit ev: Numeric[A]): List[A] =
      self.foldRight(Nil: List[A])((a, b) =>
        Cons(implicitly[Numeric[A]].plus(a, implicitly[Numeric[A]].one), b))

    /** exercise 3.17 */
    def doublesToString(implicit ev: A =:= Double): List[String] =
      self.foldRight(Nil: List[String])((a, b) => Cons(ev(a).toString, b))

    /** exercise 3.18 */
    def map[B](f: A => B): List[B] =
      self.foldRight(Nil: List[B])((a, b) => Cons(f(a), b))

    /** exercise 3.19 */
    def filter(f: A => Boolean): List[A] =
      self.foldRight(Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

    def removeOdd(implicit ev: Integral[A]): List[A] =
      self.filter(a => {
        val int = implicitly[Integral[A]]
        int.rem(a, int.fromInt(2)) == int.zero
      })

    /** exercise 3.20 */
    def flatMap[B](f: A => List[B]): List[B] =
      self.foldRight(Nil: List[B])((a, b) => f(a).append(b))

    /** exercise 3.21 */
    def flatMapFilter(p: A => Boolean): List[A] =
      self.flatMap(a => if (p(a)) List(a) else Nil)

    /** exercise 3.22 */
    def zipAdd(that: List[A])(implicit numeric: Numeric[A]): List[A] = {
      @tailrec def go(a: List[A], b: List[A], acc: List[A]): List[A] =
        (a, b) match {
          case (Cons(a, as), Cons(b, bs)) => go(as, bs, Cons(numeric.plus(a, b), acc))
          case _ => acc
        }

      go(self, that, Nil)
    }

    /** exercise 3.23 */
    def zipWith[B](that: List[A])(f: (A, A) => B): List[B] = {
      @tailrec def go(a: List[A], b: List[A], acc: List[B]): List[B] =
        (a, b) match {
          case (Cons(a, as), Cons(b, bs)) => go(as, bs, Cons(f(a, b), acc))
          case _ => acc
        }

      go(self, that, Nil)
    }

    /** exercise 3.24 */
    def hasSubsequence(sub: List[A]): Boolean = sub match {
      case Nil => false
      case Cons(head, _) =>
        self.dropWhile(_ != head).zipWith(sub)(_ == _).foldLeft(true)(_ && _)
    }
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

  def main(args: Array[String]): Unit = {
    val intLists = Array(
      List(),
      List(1),
      List(1, 2),
      List(2, 1),
      List(1, 2, 5, 1, 2)
    )

    println("tail")
    intLists.map(_.tailOption).foreach(println)

    println("\nsetHead")
    intLists.zip(Array(1, 2, 3)).map { case (list, a) => list.setHead(a) }.foreach(println)

    println("\ndrop")
    intLists.zip(Array(1, 2, 1, 2)).map { case (list, a) => list.drop(a) }.foreach(println)

    println("\ndropWhile")
    val lessThan5: Int => Boolean = _ < 5
    intLists.map(_.dropWhile(lessThan5)).foreach(println)

    println("\ninit")
    intLists.map(_.init).foreach(println)

    val doubleLists = Array(
      List(),
      List(1.0, 2.0),
      List(0.0),
      List(2.5, 4.398234, 0.1, 4.9),
      List(2.5, 4.398234, 0.1, 845.29384798, 0.0, 4589),
    )

    println("\nproductEarlyExit")
    doubleLists.map(_.productEarlyExit).foreach(println)

    println("\npassing Nil & Cons to foldRight")
    println(exercise8)

    println("\nlist length")
    doubleLists.map(_.length).foreach(println)

    println("\nleft funcs")
    println("sum")
    intLists.map(_.leftSum).foreach(println)
    println("product")
    intLists.map(_.leftProduct).foreach(println)
    println("length")
    intLists.map(_.leftLength).foreach(println)

    println("\nreverse")
    intLists.map(_.reverse).foreach(println)

    println("\nleftFoldRight")
    intLists.map(_.foldRight(Nil: List[Int])(Cons(_, _))).foreach(println)

    println("\nappend")
    intLists.zip(doubleLists).map { case (a, b) => a.append(b) }.foreach(println)

    println("\nconcatenate")
    println(concatenate(List(List(1, 2, 3), List(4, 5), List(6))))

    println("\nincrement")
    intLists.map(_.increment).foreach(println)

    println("\ndoublesToString")
    doubleLists.map(_.doublesToString).foreach(println)

    println("\nmap")
    intLists.map(_.map(_ + 1)).foreach(println)
    doubleLists.map(_.map(_.toString)).foreach(println)

    println("\nfilter")
    intLists.map(_.removeOdd).foreach(println)

    println("\nflatMap")
    println(List(1, 2, 3).flatMap(i => List(i, i)))

    println("\nflatMapFilter")
    intLists.map(_.flatMapFilter(_ % 2 == 0)).foreach(println)

    println("\nzipAdd")
    doubleLists.zip(intLists.map(_.map(_.toDouble))).map { case (a, b) => a.zipAdd(b) }.foreach(println)

    println("\nhasSubsequence")
    println(List(1, 2, 3, 4, 5).hasSubsequence(List(4, 5)))
  }
}
