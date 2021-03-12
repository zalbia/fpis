package part1.chapter2.ex4

object Uncurry {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
}
