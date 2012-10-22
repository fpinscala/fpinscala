def unit[A](a: => A): Gen[A] = 
  Gen(State.unit(a), Stream(a))

def boolean: Gen[Boolean] = 
  Gen(State(RNG.double).map(_ > .5), Stream(true,false))

def choose(start: Int, stopExclusive: Int): Gen[Int] = 
  Gen(State(RNG.positiveInt).map(n => start + n % (stopExclusive-start)),
      Stream.from(start).take(stopExclusive-start)) 

/* To define `listOfN`, we first define a helper function, `cartesian`, which generates all possible combinations of a `Stream[Stream[A]]`. For instance: 

  cartesian(Stream(Stream(1,2), Stream(3), Stream(4,5))) ==
  Stream(Stream(1,3,4), Stream(1,3,5), Stream(2,3,4), Stream(2,3,5))
Some careful handling of */
def cartesian[A](s: Stream[Stream[A]]): Stream[Stream[A]] = 
  s.foldRight(Stream(Stream[A]()))((hs,ts) => 
    for { h <- hs; t <- ts } yield Stream.cons(h,t)) 

/** Generate lists of length n, using the given generator. */
def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
  Gen(State.sequence(List.fill(n)(g.sample)),
      cartesian(Stream.constant(g.exhaustive).take(n)).map(_.toList))