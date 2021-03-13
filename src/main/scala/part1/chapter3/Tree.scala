package part1.chapter3

sealed trait Tree[+A] {
  self =>

  import Tree._

  /** exercise 3.25 */
  def size: Int = self match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + left.size + right.size
  }

  /** exercise 3.26 */
  def max[B >: A](implicit ord: Ordering[B]): B = self match {
    case Leaf(value) => value
    case Branch(left, right) => ord.max(left.max[B], right.max[B])
  }

  /** exercise 3.27 */
  def depth: Int = self match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + math.max(left.depth, right.depth)
  }

  /** exercise 3.28 */
  def map[B](f: A => B): Tree[B] = self match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(left.map(f), right.map(f))
  }

  /**
   * exercise 3.29
   * <p></p>
   * In `List`, `foldLeft` & `foldRight` have different semantics owing to
   * `List`'s right association.
   * <p></p>
   * In `Tree`, there can only be one fold as the structure is symmetrical.
   * <p></p>
   * A fold on a structure mirrors the structure itself.
   */
  def fold[B](f: A => B)(g: (B, B) => B): B = self match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(left.fold(f)(g), right.fold(f)(g))
  }

  def reduce[B >: A](f: (B, B) => B): B =
    fold(identity[B])(f)

  def foldSize: Int =
    fold(_ => 1)((b, c) => 1 + b + c)

  def foldMax[B >: A](implicit ord: Ordering[B]): B =
    reduce(ord.max[B])

  def foldDepth: Int =
    fold(_ => 0)((b, c) => 1 + math.max(b, c))

  def foldMap[B](f: A => B): Tree[B] =
    fold[Tree[B]](f.andThen(Leaf(_)))(Branch(_, _))
}

object Tree {

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}
