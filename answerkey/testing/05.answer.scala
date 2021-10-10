def unit[A](a: => A): Gen[A] =
  State.unit(a)

def boolean: Gen[Boolean] =
  State(RNG.boolean)

extension [A](self: Gen[A]) def listOfN[A](n: Int): Gen[List[A]] =
  State.sequence(List.fill(n)(self))
