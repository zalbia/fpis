package part1.chapter2.ex5

object Compose {
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
