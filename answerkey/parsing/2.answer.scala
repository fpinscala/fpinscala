def many1[A](p: Parser[A]): Parser[List[A]] = 
  map2(p, many(p))(_ :: _)