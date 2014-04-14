def from(n: Int): Stream[Int] =
  cons(n, from(n+1))