extension [A](p: Parser[A])
  def product[B](p2: => B): Parser[(A, B)] =
    for
      a <- p
      b <- p2
    yield (a, b)

  def map2[B, C](p2: => B)(f: (A, B) => C): Parser[C] =
    for
      a <- p
      b <- p2
    yield f(a, b)