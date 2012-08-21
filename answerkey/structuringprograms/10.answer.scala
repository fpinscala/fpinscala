def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E =
  a => f(g(a), h(a), i(a))