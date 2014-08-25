/* 
These can be implemented using a for-comprehension, which delegates to the `flatMap` and `map` implementations we've provided on `ParserOps`, or they can be implemented in terms of these functions directly. 
*/
def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = 
  flatMap(p)(a => map(p2)(b => (a,b)))

def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = 
  for { a <- p; b <- p2 } yield f(a,b)