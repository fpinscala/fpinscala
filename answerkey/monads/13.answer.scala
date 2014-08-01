def flatMap[A,B](ma: F[A])(f: A => F[B]) =
  join(map(ma)(f))

def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
  a => join(map(f(a))(g))
