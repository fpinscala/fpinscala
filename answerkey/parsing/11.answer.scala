def map[A,B](a: Parser[A])(f: A => B): Parser[B] = 
  flatMap(a)(f andThen succeed)