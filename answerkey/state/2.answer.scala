// We generate an integer >= 0 and divide it by one higher than the
// maximum. This is just one possible solution.
def double(rng: RNG): (Double, RNG) = {
  val (i, r) = nonNegativeInt(rng)
  (i / (Int.MaxValue.toDouble + 1), r)
}