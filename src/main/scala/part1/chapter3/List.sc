import part1.chapter3.List
import part1.chapter3.List._

val intLists = Array(
  List(),
  List(1),
  List(1, 2),
  List(2, 1),
  List(1, 2, 5, 1, 2)
)

// match expression result
exercise1 // 3

// tail
intLists.map(_.tailOption)

// setHead
intLists.zip(Array(1, 2, 3)).map { case (list, a) => list.setHead(a) }

// drop
intLists.zip(Array(1, 2, 1, 2)).map { case (list, a) => list.drop(a) }

// dropWhile
val lessThan5: Int => Boolean = _ < 5
intLists.map(_.dropWhile(lessThan5))

// init
intLists.map(_.init)

val doubleLists = Array(
  List(),
  List(1.0, 2.0),
  List(0.0),
  List(2.5, 4.398234, 0.1, 4.9),
  List(2.5, 4.398234, 0.1, 845.29384798, 0.0, 4589),
)

// productEarlyExit
doubleLists.map(_.productEarlyExit)

// passing Nil & Cons to foldRight
exercise8

// list length
doubleLists.map(_.length)

// left funcs
// sum
intLists.map(_.leftSum)
// product
intLists.map(_.leftProduct)
// length
intLists.map(_.leftLength)

// reverse
intLists.map(_.reverse)

// leftFoldRight
intLists.map(_.foldRight(Nil: List[Int])(Cons(_, _)))

// append
intLists.zip(doubleLists).map { case (a, b) => a.append(b) }

// concatenate
concatenate(List(List(1, 2, 3), List(4, 5), List(6)))

// increment
intLists.map(_.increment)

// doublesToString
doubleLists.map(_.doublesToString)

// map
intLists.map(_.map(_ + 1))
doubleLists.map(_.map(_.toString))

// filter
intLists.map(_.removeOdd)

// flatMap
List(1, 2, 3).flatMap(i => List(i, i))

// flatMapFilter
intLists.map(_.flatMapFilter(_ % 2 == 0))

// zipAdd
doubleLists.zip(intLists.map(_.map(_.toDouble))).map { case (a, b) => a.zipAdd(b) }

// hasSubsequence
List(1, 2, 3, 4, 5).hasSubsequence(List(4, 5))