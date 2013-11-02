def flatMap[A,B](ma: F[A])(f: A => F[B]) =
  join(map(ma)(f))