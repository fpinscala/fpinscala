def curry[A,B,C](f: (A, B) => C): A => B => C =
  a => b => f(a, b)

/* The `Function2` trait has a `curried` method already. */