  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for { a <- p; b <- p2 } yield f(a,b)

