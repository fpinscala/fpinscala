def from(n: Int): LazyList[Int] =
  cons(n, from(n+1))