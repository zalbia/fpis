import part1.chapter5.LazyList
import part1.chapter5.LazyList._

val lazyListFive = LazyList(1, 2, 3, 4, 5)

LazyList.empty.headOption
lazyListFive.headOption

lazyListFive.toList

LazyList.empty.take(3).toList
lazyListFive.take(3).toList

LazyList.empty.takeWhile((_: Nothing) => true).toList
lazyListFive.takeWhile(_ % 2 == 1).toList
lazyListFive.takeWhile(_ < 3).toList

LazyList.empty.exists((_: Nothing) => true)
lazyListFive.exists(_ == 3)

LazyList.empty[Nothing].foldRight(())((_, b) => b)
lazyListFive.foldRight(0)(_ + _)

LazyList.empty.takeWhileFoldRight((_: Nothing) => true).toList
lazyListFive.takeWhileFoldRight(_ % 2 == 1).toList
lazyListFive.takeWhileFoldRight(_ < 3).toList

LazyList.empty.headOptionFoldRight
lazyListFive.headOptionFoldRight

LazyList.empty.map((_: Nothing).toString).toList
lazyListFive.map(a => a * a).toList

LazyList.empty.filter((_: Nothing) => true).toList
lazyListFive.filter(_ % 2 == 1).toList
lazyListFive.filter(_ < 3).toList

LazyList.empty.append(1).toList
lazyListFive.append(6).toList

LazyList.empty.reverse.toList
lazyListFive.reverse.toList

LazyList.empty.concat(lazyListFive).toList
Empty.concat(Empty).toList
lazyListFive.concat(lazyListFive).toList

LazyList.empty.flatMap((_: Nothing) => lazyListFive).toList
(for (a <- lazyListFive ; b <- lazyListFive) yield a + b).toList

val ones: LazyList[Int] = LazyList.cons(1, ones)

ones.take(5).toList
ones.exists(_ % 2 != 0)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1).take(3).toList // Stack overflow without take(3)
ones.forAll(_ != 1)

val ones: LazyList[Int] = LazyList.constant(1)

ones.take(5).toList
ones.exists(_ % 2 != 0)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1).take(3).toList // Stack overflow without take(3)
ones.forAll(_ != 1)

from(2)
from(Int.MaxValue - 500).take(1000).foreach(println)

fibs
fibs.take(10).toList

Unfold.fibs
Unfold.fibs.take(10).toList

Unfold.from(2)
Unfold.from(Int.MaxValue - 500).take(1000).foreach(println)

lazyListFive.UseUnfold.map(_ + 1).toList
lazyListFive.UseUnfold.take(3).toList
lazyListFive.UseUnfold.takeWhile(_ <= 3).toList
lazyListFive.UseUnfold.takeWhile(_ % 2 == 1).toList
lazyListFive.UseUnfold.takeWhile(_ % 2 == 0).toList
lazyListFive.UseUnfold.zipWith(lazyListFive)((_, _)).toList
lazyListFive.UseUnfold.zipWith(Empty)((_, _)).toList
lazyListFive.UseUnfold.zipWith(lazyListFive.take(3))((_, _)).toList
lazyListFive.take(3).UseUnfold.zipWith(lazyListFive)((_, _)).toList
lazyListFive.UseUnfold.zipAll(lazyListFive.take(3)).toList

lazyListFive.take(3).startsWith(lazyListFive)
lazyListFive.startsWith(lazyListFive.take(3))
lazyListFive.startsWith(LazyList(1))
lazyListFive.startsWith(LazyList(2))

lazyListFive.tails.map(_.toList).toList

lazyListFive.hasSubsequence(lazyListFive.take(3))
lazyListFive.hasSubsequence(LazyList(3, 4, 5))
lazyListFive.hasSubsequence(LazyList(3, 4))
lazyListFive.hasSubsequence(lazyListFive.append(6))
lazyListFive.append(6).hasSubsequence(lazyListFive)
lazyListFive.hasSubsequence(LazyList(-1))

lazyListFive.take(3).scanRight(0)(_ + _).toList
