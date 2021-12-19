extension [A](ffa: F[F[A]]) def join: F[A] =
  ffa.flatMap(identity)
