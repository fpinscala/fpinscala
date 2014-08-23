def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
  a => flatMap(f(a))(g)