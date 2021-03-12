package part1.chapter2.ex1

import scala.annotation.tailrec

object Fib {

  def fib(n: Int): Int = {
    @tailrec
    def loop(a: Int, b: Int, i: Int): Int = {
      if (i == n) a
      else loop(b, a + b, i + 1)
    }

    loop(0, 1, 1)
  }

  def main(args: Array[String]): Unit = {
    (1 to 10).map(fib).foreach(println)
  }
}
