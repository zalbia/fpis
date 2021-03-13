package part1.chapter4

sealed trait Either[+E, +A] {
  self =>

  import Either.{Left, Right}

  // exercise 4.6
  def flatMap[E1 >: E, B](f: A => Either[E1, B]): Either[E1, B] = self match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def map[B](f: A => B): Either[E, B] =
    flatMap(a => Right(f(a)))

  def fold[B](z: B)(f: A => B): B =
    map(f) match {
      case Left(_) => z
      case Right(b) => b
    }

  def orElse[E1 >: E, B >: A](b: => Either[E1, B]): Either[E1, B] =
    fold(b)(Right(_))

  def map2[E1 >: E, B, C](b: Either[E1, B])(f: (A, B) => C): Either[E1, C] =
    for {
      a <- self
      b <- b
    } yield f(a, b)

  // end of exercise 4.6

  def mapError[E1](f: E => E1): Either[E1, A] = self match {
    case Left(e) => Left(f(e))
    case right: Right[A] => right
  }
}

object Either {

  import Option._

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    Try(x / y)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  // exercise 4.7
  def traverse[E, A](es: List[Either[E, A]])(f: A => Either[E, A]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(List.empty))((a, b) => a.map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(Right(_))

  // end of exercise 4.7

  /**
   * exercise 4.8
   * <p></p>
   * <em>What would you need to change in order to report both errors?</em>
   * <p></p>
   * You would need to make, say, a `map2Ap`, which takes another function `g`
   * that combines two errors into one.
   * <p></p>
   * <em>Would you change `map2` or the signature of `mkPerson`?</em>
   * <p></p>
   * You can change either, or both. By defining `map2Ap`, you could pass a way
   * to combine two `E`'s or just put them in a tuple.
   * * <p></p>
   * <em>
   * Or could you create a new data type that captures this requirement better
   * than `Either` does, with some additional structure? How would `orElse`,
   * `traverse`, and `sequence` behave differently for that data type?
   * </em>
   * <p></p>
   * Yes, you definitely could, and that already exists in various
   * implementations of the `Validation[+E, +A]` type.
   * <p></p>
   * Using `orElse` would allow one to extract a tuple out errors out of each of
   * two `Validation`s. Functions `traverse`, and `sequence` would instead
   * gather all errors in some data type with a `Traversable` instance.
   */
  def map2Ap[E, E1, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C)(
    g: (Option[E], Option[E]) => E1
  ): Either[E1, C] =
    (a, b) match {
      case (Left(e1), Left(e2)) => Left(g(Some(e1), Some(e2)))
      case (Left(e), Right(_)) => Left(g(Some(e), None))
      case (Right(_), Left(e)) => Left(g(None, Some(e)))
      case (Right(a), Right(b)) => Right(f(a, b))
    }

  def map2ErrorMkString[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C)(
    implicit ev: E <:< String
  ): Either[String, C] =
    map2Ap(a, b)(f) {
      case (None, None) => ""
      case (None, Some(e)) => ev(e)
      case (Some(e), None) => ev(e)
      case (Some(e1), Some(e2)) => ev(e1) + "\n" + ev(e2)
    }

  case class Person(name: Name, age: Age)

  sealed abstract case class Name(value: String)

  sealed abstract case class Age(value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name) {})

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age) {})

  def mkPerson(name: String, age: Int): Either[String, Person] =
    map2ErrorMkString(mkName(name), mkAge(age))(Person)

  def main(args: Array[String]): Unit =
    List(
      mkPerson("", -1),
      mkPerson("Foo", -1),
      mkPerson(null, 0),
      mkPerson("Foo", 0)
    ).foreach(println)
}
