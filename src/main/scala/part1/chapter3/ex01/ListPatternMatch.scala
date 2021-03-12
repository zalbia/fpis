package part1.chapter3.ex01

import part1.chapter3.List.sum
import part1.chapter3._

object ListPatternMatch {
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
  }
}
