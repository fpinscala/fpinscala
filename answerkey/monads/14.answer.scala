def flatMap[A,B](ma: M[A])(f: A => M[B]) =
  join(map(ma)(f))