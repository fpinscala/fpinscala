def choose(start: Int, stopExclusive: Int): Gen[Int] =
  Gen(State(RNG.positiveInt).map(n => start + n % (stopExclusive-start)))

/* We could write this as an explict state action, but this is far less
   convenient, since it requires us to manually thread the `RNG` through the
   computation. */
def choose2(start: Int, stopExclusive: Int): Gen[Int] = 
  Gen(State(rng => RNG.positiveInt(rng) match {
    case (n,rng2) => (start + n % (stopExclusive-start), rng2)
  }))