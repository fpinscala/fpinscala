// `Int.MinValue` is a corner case that needs special handling
// since its absolute value doesn't fit in an `Int`.
// We could just select `Int.MaxValue` or `0` as a replacement
// but that would skew the generator. One solution is to simply
// retry recursively until we get a different number.
def positiveInt(rng: RNG): (Int, RNG) = {
  val (i, r) = rng.nextInt
  if (i == Int.MinValue) positiveInt(r) else (i.abs, r)
}