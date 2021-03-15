import part1.chapter5.RNG.{SimpleRNG, double, intDouble, nonNegativeInt}

val rng = SimpleRNG(42)

val (n1, rng2) = rng.nextInt

val (n2, rng3) = rng2.nextInt

val (n3, rng4) = nonNegativeInt(rng3)

val (n4, rng5) = nonNegativeInt(rng4)

val (d1, rng6) = double(rng5)
val (d2, rng7) = double(rng6)
val (d3, rng8) = double(rng7)
val (d3, rng9) = double(rng8)
val (d3, rng10) = double(rng9)
val (d3, rng11) = double(rng10)
val (d3, rng12) = double(rng11)

val (nd, rng13) = intDouble(rng12)
