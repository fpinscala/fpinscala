// We need to be quite careful not to skew the generator.
// Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
// it suffices to increment the negative numbers by 1 and make them positive.
// This maps Int.MinValue to Int.MaxValue and -1 to 0.
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i, r) = rng.nextInt
  (if (i < 0) -(i + 1) else i, r)
}