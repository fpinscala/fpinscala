def unit[A](a: => A): Gen[A] =
  Gen(State.unit(a))

def boolean: Gen[Boolean] =
  Gen(State(RNG.boolean))

def choose(start: Int, stopExclusive: Int): Gen[Int] =
  Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
  Gen(State.sequence(List.fill(n)(g.sample)))