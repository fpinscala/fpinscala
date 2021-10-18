extension [A](p: Parser[A])
  def map[B](f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))