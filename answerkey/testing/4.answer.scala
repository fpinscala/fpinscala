def choose(start: Int, stopExclusive: Int): Gen[Int] = 
  State(RNG.postitiveInt).map(n => start + n % (stopExclusive-start))

/* We could write this as an explict state action, but this is less idiomatic, since it requires us to manually thread the `RNG` through the computation. */
def choose2(start: Int, stopExclusive: Int): Gen[Int] = 
  rng => RNG.positiveInt(rng) match { case (n,rng2) => (start + n % (stopExclusive-start), rng2) }