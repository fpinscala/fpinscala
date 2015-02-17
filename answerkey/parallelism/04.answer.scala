def asyncF[A,B](f: A => B): A => Par[B] =
  a => lazyUnit(f(a))
