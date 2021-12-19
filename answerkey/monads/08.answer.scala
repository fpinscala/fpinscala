extension [A](fa: F[A])
  def flatMap[B](f: A => F[B]): F[B] =
    compose(_ => fa, f)(())