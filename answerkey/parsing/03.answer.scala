extension [A](p: Parser[A])
  def many: Parser[List[A]] =
    p.map2(p.many)(_ :: _) | succeed(Nil)


