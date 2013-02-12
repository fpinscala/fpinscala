def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
  a => flatMap(f(a))(g)