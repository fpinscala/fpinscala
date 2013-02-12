def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
  compose((_:Unit) => ma, f)(())