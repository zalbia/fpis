package part1.chapter5

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /** exercise 6.1  */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    if (n == Int.MinValue)
      (Int.MaxValue, nextRNG)
    else if (n < 0)
      (-n, nextRNG)
    else
      (n, nextRNG)
  }

  /** exercise 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = rng.nextInt
    ((n.toDouble / Int.MaxValue.toDouble / 2) + 0.5, nextRNG)
  }

  /** exercise 6.3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }
}
