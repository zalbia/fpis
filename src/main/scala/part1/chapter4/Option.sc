import part1.chapter4.Option
import part1.chapter4.Option._

import scala.util.Try

// flatMap
for (a <- Some("a") ; b <- Some("b")) yield a + b

// map
Some(1).map(_ + 1)

// getOrElse
None.getOrElse(1)
Some(0).getOrElse(1)

// orElse
None.orElse(Some(1))
Some(0).orElse(Some(1))
Some(0).orElse(None)

val doubles = Seq(1.0, 2.0, 3.0)

// variance
variance(Seq.empty)
variance(doubles)

// map2
map2(Some(1), Some(2))(_ + _)

val listOpts = List(
  List(None, Some(2)),
  List(Some(1), Some(2))
)

// sequence
listOpts.map(sequence)

sequence(List(None, Some(2)))
sequence(List(Some(1), Some(2)))

// traverse
def parseInts(a: List[String]): Option[List[Int]] =
  traverse(a)(i => Try(i.toInt).toOption match {
    case scala.Some(value) => Some(value)
    case scala.None => None
  })
List(
  List(),
  List(""),
  List("a"),
  List("1", "2", "3"),
).map(parseInts)

// traverseSequence
listOpts.map(traverseSequence)