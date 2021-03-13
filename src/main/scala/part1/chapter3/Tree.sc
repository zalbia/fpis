import part1.chapter3.Tree._

val trees = Array(
  Leaf(1),
  Branch(Leaf(2), Leaf(4)),
  Branch(Branch(Leaf(3), Leaf(1)), Branch(Leaf(5), Leaf(2))),
  Branch(Branch(Leaf(3), Leaf(1)), Leaf(2)),
)

// size
trees.map(_.size)

// max
trees.map(_.max)

// depth
trees.map(_.depth)

// map
trees.map(_.map(_ + 1))

// folds

// foldSize
trees.map(_.foldSize)
// foldMax
trees.map(_.foldMax)
// foldDepth
trees.map(_.foldDepth)
// foldMap
trees.map(_.map(_ + 1))