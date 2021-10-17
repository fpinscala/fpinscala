extension [A](p: Parser[A])
  def listOfN(n: Int): Parser[List[A]] =
    if n <= 0 then succeed(Nil)
    else p.map2(p.listOfN(n - 1))(_ :: _)