package fpinscala.state

// ch 6.2 purely functional random number generation
// ch 6.3 making stateful apis pure
trait RNG {
  def nextInt: (Int, RNG)
  // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  // AND return the new state alongside
}

object RNG {
  // NB - this was called SimpleRNG in the book text
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def main(args: Array[String]): Unit = {
    println("RNG.main()")

    val rng = Simple(42)
    val rng1 = rng.nextInt
    println(rng1)
    println(rng.nextInt) // this second trigger on the original state should be the same as rng1

    val rng2 = rng1._2.nextInt
    println(rng2)

    println(rng2._2.nextInt)

    println("nonNegativeInt:" + nonNegativeInt(rng))

    println("double3:" + double3(rng))

    println("ints:" + ints(5)(rng))
    // ints:(List(16159453, -1281479697, -340305902, -2015756020, 1770001318),Simple(115998806404289))
    println("ints2:" + ints2(5)(rng))
    // ints2:(List(1770001318, -2015756020, -340305902, -1281479697, 16159453),Simple(115998806404289))
    // note ints and ints2 element order are reversed !!!
  }

  // val int: (Int, RNG) = nextInt

  // We need to be quite careful not to skew the generator.
  // Since `Int.MinValue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // We generate an integer >= 0 and divide it by one higher than the
  // maximum. This is just one possible solution.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def boolean(rng: RNG): (Boolean, RNG) = rng.nextInt match {
    case (i,rng2) => (i%2==0,rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  // A simple recursive solution
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  // A tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }
}
