def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
  map(product(a,b)) { case (a,b) => f(a,b) }