// This implementation of map2 passes the initial RNG to the first argument
// and the resulting RNG to the second argument. It's not necessarily wrong
// to do this the other way around, since the results are random anyway.
// We could even pass the initial RNG to both `f` and `g`, but that might
// have unexpected results. E.g. if both arguments are `RNG.int` then we would
// always get two of the same `Int` in the result. When implementing functions
// like this, it's important to consider how we would test them for
// correctness.
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }