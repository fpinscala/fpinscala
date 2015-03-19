package fpinscala.testing.exhaustive

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def unit[A](a: A): Rand[A] = State.unit(a)

  def int: Rand[Int] = State { _.nextInt }

  def nonNegativeInt: Rand[Int] = int.map { i =>
    if (i < 0) -(i + 1) else i
  }

  def double: Rand[Double] = nonNegativeInt.map { i =>
    i / (Int.MaxValue.toDouble + 1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  def stream[A](a: Rand[A])(rng1: RNG): Stream[A] =
    Stream.iterate(a.run(rng1)) { case (_, rng2) => a.run(rng2)  }
      .map(_._1)

}