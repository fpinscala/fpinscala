def map2[A,B,C](p: Parser[A], p2: Parser[B])(
                f: (A,B) => C): Parser[C] = 
  map(product(p, p2))(f.tupled)
def many1[A](p: Parser[A]): Parser[List[A]] = 
  map2(p, many(p))(_ :: _)