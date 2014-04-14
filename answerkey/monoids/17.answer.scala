def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
  new Monoid[A => B] {
    def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
    val zero: A => B = a => B.zero
  }